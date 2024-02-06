(define-module (scm-bin qemu-vm)
;;; All used modules must be present in the module (srvc scheme-files) under:
;;;   service-file -> with-imported-modules
  ;; #:use-module (memo) ;; is-system-ecke is-system-edge
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 getopt-long) ;; see also `info "guile-config"'
  #:export (main))

;;;;;; Code duplication ;;;;;;
;;; TODO look at other scripts in the scm-bin, how they use the `utils' module.

;;;;;; begin: from utils.scm
(define (partial fun . args)
  (lambda x (apply fun (append args x))))

(define (dbg-exec prm)
  "`pk', i.e. `peek' can be used instead of this function"
  ;; TODO implement pretty-print for bash commands
  ;; ~a - outputs an argument like display
  ;; ~s - outputs an argument like write (i.e. print to string)
  ;; ~% is newline \n
  (format #t "$ ~a\n" prm)
  prm)

(define (cmd->string cmd)
  (dbg-exec
   (if (list? cmd)
       (string-join cmd) ;; join with ' ' by default
       cmd)))

(define (read-all-strings port)
  "Return a list of all lines of text from the PORT.
Returns a list of strings"
  (let loop ((res '())
             (str (read-line port))) ; from (ice-9 readline)
    (if (and str (not (eof-object? str)))
        (loop (append res (list str))
              (read-line port))
        res)))

(define (exec command)
  "Run the shell COMMAND using ‘/bin/sh -c’ with ‘OPEN_READ’ mode, ie. to read
from the subprocess. Wait for the command to terminate and return a string
containing its output.

TODO have a look if a delimited continuation can be used to break out of `exec',
i.e. skip the `read-all-strings' and thus make `exec-background' out of it.

Usage:
(let* ((ret (exec command)))
    (if (= 0 (car ret))
        (let* ((output (cdr ret)))
          #| process output |#)
      (begin
        (format #t \"~a\\n\" (error-command-failed))
        *unspecified*)))"
  ;; ,use (guix build utils) contains `invoke'
  ;; `invoke' does `(apply system* program args)'; `system*' waits for the program
  ;; to finish, The command is executed using fork and execlp.

  ;; TODO
  ;; Scheme Procedure: chdir str
  ;; Change the current working directory to str. The return value is unspecified.

  ((compose
    (lambda (command)
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             (str  (read-all-strings port)))
        (cons
         (status:exit-val (close-pipe port))
         str)))
    cmd->string)
   command))
;;;;;; end: from utils.scm

(define vmRAM (format #f "~aG" 16))
;; The HDD can be resized using `qemu-image'. See notes.
(define vmHDDSize (format #f "~aG" 30))
(define vmRemoteViewPort "5930")
(define vmSSHPort "10022")
(define vmApache2Port "10080")
(define (vmCPUCores user)
  "The VM gets all available CPUs. This script runs as root, however the $cores
environment variable is not available for the root user."
  ((compose
    (partial format #f "cpus=~s")
    number->string)
   ((@(ice-9 threads) current-processor-count))))

#|
See https://www.qemu.org/docs/master/system/qemu-cpu-models.html
How do I get AVX support in QEMU?
https://superuser.com/a/454814

(is-system-ecke):
EPYC-v1  AMD EPYC Processor              ;; works
EPYC-v2  AMD EPYC Processor (with IBPB)  ;; works
EPYC-v3  AMD EPYC Processor              ;; doesn't work
Opteron_G1 ;; test it
Opteron_G2 ;; test it
Opteron_G3 ;; test it
Opteron_G4 ;; test it
Opteron_G5 ;; test it
phenom     ;; test it

Using `-cpu host` may cause troubles when moving between machines,e.g. host A ->
host B, i.e. when on host A something is compiled using some CPU instruction
which is not supported by the CPU on the host B
|#
(define vmCPUModel
  (cond
   #;((is-system-ecke) "EPYC-v1,+avx,enforce")
   #;((is-system-edge) "SapphireRapids" #;"SapphireRapids,+avx") ;; both work
   (#t "host")))

(define ssh
  (format
   #f
   "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p ~a"
   vmSSHPort))

(define (user-localhost user)
  (format #f "~a@localhost" user))

(define (usage vmUser)
  (format #f
          "~a\n~a\n~a"
          (format #f
                  "
### When: Could not access KVM kernel module: Permission denied
###     sudo usermod --append --groups kvm $USER

# Open a new terminal on the (Ubuntu) host and connect with:
   remote-viewer spice://localhost:~a & disown"
                  vmRemoteViewPort)
          (format #f
                  "
# or with SSH on Ubuntu:
   sudo apt install openssh-server:
# with SSH on the host:
   ~a ~a" ssh (user-localhost vmUser))
          (format #f
                  "
# File transfer example:
  set remoteShell \"~a\"
  set noWarn \"Permanently added '\\[localhost\\]:~a'\"
  rsync -avz --rsh=\"$remoteShell\" /tmp/foo ~a:/tmp/ 2>&1 | grep -v \"$noWarn\"
" ssh vmSSHPort (user-localhost vmUser))))

(define* (start-vm user #:key qcow2File (isoFile #f))
  "TODO Auto-detect qcow2File / isoFile according to file extension"
  ((compose
    exec
    (lambda (args)
      (append
       (list
        ;; This works, however without shared clipboard:
        ;; qemu-system-x86_64 \
        ;;     -nic user,model=virtio-net-pci \
        ;;     -enable-kvm -m $vmRAM \
        ;;     -device virtio-blk,drive=myhd \
        ;;     -drive if=none,file=$qcow2File,id=myhd \
        ;;     & disown
        ;;
        ;; With shared clipboard and SSH access:
        "qemu-system-x86_64"
        "-vga" "virtio"  ;; video card type
        "-smp" (vmCPUCores user))
       (if isoFile (list "-cdrom" isoFile) (list))
       (list
        "-nic"
        (format
         #f
         "user,model=virtio-net-pci,hostfwd=tcp::~a-:22,hostfwd=tcp::~a-:80"
         vmSSHPort vmApache2Port)
        "-enable-kvm" "-m" vmRAM
        "-device" "virtio-blk,drive=myhd"
        "-cpu" vmCPUModel
        "-drive" (string-append "if=none,file=" qcow2File ",id=myhd")
;;; Access the VM with spice remote-viewer. With clipboard sharing. See `usage'
        ;;  remote-viewer spice://localhost:5930 & disown
        (string-join
         (list
          "-device"
          "virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5"
          "-chardev" "spicevmc,name=vdagent,id=vdagent"
          "-device"
          (string-append
           "virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent"
           ",name=com.redhat.spice.0")
          "-spice" (string-append "port=" vmRemoteViewPort ",disable-ticketing=on")
          ) " ")
        ;; "&" "disown" ;; TODO where is disown located???
        )))
    exec
    (lambda ()
      (if (access? qcow2File W_OK)
          (begin
            (format #t "DBG: qcow2File ~a exists and is writable\n" qcow2File)
            (list))
          (list
           "sudo" (string-append "--user=" user)
           "qemu-img" "create" "-f" "qcow2" qcow2File vmHDDSize))))))

(define (main args)
  (let* [
         (option-spec
          '(
            ;; (value #t): a given option expects accept a value
            ;; 'user' corresponds to '--user' or '-u' on the command line
            (user       (single-char #\u) (value #t))
            (iso-file   (single-char #\i) (value #t))
            (qcow2-file (single-char #\q) (value #t))
            ;; (value #f): a given option doesn't accept a value
            (version    (single-char #\v))
            (help       (single-char #\h))))

         (options (getopt-long args option-spec))
         ;; #f means that the expected value wasn't specified
         (user           (option-ref options 'user #f))
         (iso-file       (option-ref options 'iso-file #f))
         (qcow2-file     (option-ref options 'qcow2-file #f))
         (version-wanted (option-ref options 'version #f))
         (help-wanted    (option-ref options 'help #f))
         ]
    ;; (format #t "\n")
    ;; (format #t "user:           ~a\n" user)
    ;; (format #t "iso-file:       ~a\n" iso-file)
    ;; (format #t "qcow2-file:     ~a\n" qcow2-file)
    ;; (format #t "version-wanted: ~a\n" version-wanted)
    ;; (format #t "help-wanted:    ~a\n" help-wanted)
    ;; (format #t "\n")

    (if (or version-wanted help-wanted)
        (begin
          ;; (if version-wanted
          ;;     (display "getopt-long-example version 0.3~%"))
          (if help-wanted
              (format #t "
      qemu-vm [options]
      -v, --version    Display version
      -h, --help       Display this help
      ~a
      "
                      (usage user))))
        (begin
          (format #t "~a~%" (usage user))
          (start-vm user
                    #:qcow2File qcow2-file
                    #:isoFile iso-file)))))
(unless (defined? 'main)
  (error (format #f "Symbol undefined: ~a" 'main)))

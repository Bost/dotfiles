;; Ubuntu 21.04 in a Virtual Machine via qemu-system-x86_64
;; https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

;; Clipboard support:
;; on the (Ubuntu) host run:
;;   sudo apt install gir1.2-spiceclientgtk-3.0 virt-viewer
;; in the VM run:
;;   guix install spice-vdagent virt-viewer
;; I guess the virt-viewer is needed only on the (Ubuntu) host, but let's put
;; it everywhere just in case.

(use-modules (ice-9 rdelim)
             (ice-9 popen)
             (ice-9 getopt-long))

(format #t "~%")

(define (exec command)
  "The command must have only one line output. TODO improve it"
  ((compose
    (lambda (command)
      (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
             (str  (read-line port))) ; from (ice-9 popen)
        (close-pipe port)
        str))
    (lambda (s)
      ;; TODO implement pretty-print for bash commands
      ;; ~a - outputs an argument like display
      ;; ~s - outputs an argument like write (i.e. print to string)
      (format #t "~a~%" s)
      s)
    (lambda (cmd) (if (list? cmd)
                      (string-join cmd " ")
                      cmd)))
   command))

(define vmRAM "4G")
(define vmHDDSize "16G")
(define vmRemoteViewPort "5930")
(define vmSSHPort "10022")
(define vmApache2Port "10080")
(define (vmCPUCores user)
  (number->string (/ (string->number
                      (exec (list "sudo" (string-append "--user=" user)
                                  "nproc"))) 2)))

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
  set noWarn \"Permanently added '[localhost]:~a'\"
  rsync -avz --rsh=\"$remoteShell\" /tmp/foo ~a:/tmp/ 2> psub | grep -v \"$noWarn\" 1>&2
" ssh vmSSHPort (user-localhost vmUser))))

(define (start-vm user isoFile qcow2File)
  ((compose
    exec
    (lambda (args)
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
       "-cdrom" isoFile
       "-nic"
       (format
        #f
        "user,model=virtio-net-pci,hostfwd=tcp::~a-:22,hostfwd=tcp::~a-:80"
        vmSSHPort vmApache2Port)
       "-enable-kvm" "-m" vmRAM
       "-device" "virtio-blk,drive=myhd"
       "-drive" (string-append "if=none,file=" qcow2File ",id=myhd")
       ;; spice remote-viewer
       #;(string-join
       (list
       "-device"
       "virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5"
       "-chardev" "spicevmc,name=vdagent,id=vdagent"
       "-device"
       (string-append
       "virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent"
       ",name=com.redhat.spice.0")
       "-spice" (string-append "port=" vmRemoteViewPort ",disable-ticketing=on")
       "-vga" "qxl"
       "-smp" (vmCPUCores user)
       ) " ")
       ;; "&" "disown" ;; TODO where is disown located???
       ))
    exec
    (lambda ()
      (if (access? qcow2File W_OK)
          (begin
            (format #t "DBG: qcow2File ~a exists and is writable~%" qcow2File)
            (list))
          (list
           "sudo" (string-append "--user=" user)
           "qemu-img" "create" "-f" "qcow2" qcow2File vmHDDSize))))))

(define (main args)
  (let* ((option-spec
          '((user     (single-char #\u) (value #t))
            (iso-file (single-char #\i) (value #t))
            (version  (single-char #\v) (value #f))
            (help     (single-char #\h) (value #f))))
         (options (getopt-long args option-spec))
         (user           (option-ref options 'user #t))
         (iso-file       (option-ref options 'iso-file #t))
         (help-wanted    (option-ref options 'help #f))
         (version-wanted (option-ref options 'version #f)))
    (if (or version-wanted help-wanted)
        (begin
          #;(if version-wanted
              (display "getopt-long-example version 0.3~%"))
          (if help-wanted
              (format #t "
ubuntu-os [options]
  -v, --version    Display version
  -h, --help       Display this help
~a
" (usage user))))
        (begin
          (format #t "~a~%" (usage user))
          (start-vm user iso-file (string-append iso-file ".qcow2"))))))

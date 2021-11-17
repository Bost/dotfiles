;; GNU Guix in a Virtual Machine via qemu-system-x86_64
;; https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

;; Clipboard support:
;; on the (Ubuntu) host run:
;;   sudo apt install gir1.2-spiceclientgtk-3.0 virt-viewer
;; in the VM run:
;;   guix install spice-vdagent virt-viewer
;; I guess the virt-viewer is needed only on the (Ubuntu) host, but let's put
;; it everywhere just in case.

(use-modules (ice-9 rdelim)
             (ice-9 popen))

(define (exec command)
  "The command must have only one line output. TODO improve it"
  (let* ((port (open-input-pipe command)) ; from (ice-9 rdelim)
         (str  (read-line port))) ; from (ice-9 popen)
    (close-pipe port)
    str))

(define guixFile
  (string-append (getenv "virtMachines")
                 "/guix-system-vm-image-1.3.0.x86_64-linux.qcow2"))
(define guixRAM "2G")
(define guixRemoteViewPort "5930")
(define guixSSHPort "10022")

(define cpuCores (number->string (/ (string->number (exec "nproc")) 2)))

(format
 #t
 (string-append
  "# Open a new terminal on the (Ubuntu) host and connect with:\n"
  "#   remote-viewer spice://localhost:~a & disown\n"
  "# or with SSH:\n"
  "#   ssh -o UserKnownHostsFile=/dev/null"
  " -o StrictHostKeyChecking=no -p ~a guest@localhost\n"
  "\n"
  "# Copy file example:\n"
  "#   scp -P ~a <srcfile> guest@localhost:</path/to/dstfile>\n")
 guixRemoteViewPort guixSSHPort guixSSHPort)

;; (format #t "\n~a\n\n" (string-join cmd " "))
((compose
  close-pipe
  open-input-pipe
  (lambda (s)
    (format #t "\n~a\n\n" s) ;; "... ~s ..." prints to string
    s)
  (lambda (cmd) (string-join cmd " ")))
 (list
  ;; This works, however without shared clipboard:
  ;; qemu-system-x86_64 \
  ;;     -nic user,model=virtio-net-pci \
  ;;     -enable-kvm -m $guixRAM \
  ;;     -device virtio-blk,drive=myhd \
  ;;     -drive if=none,file=$guixFile,id=myhd \
  ;;     & disown
  ;;
  ;; With shared clipboard and SSH access:
  "qemu-system-x86_64"
  "-nic" (string-append
          "user,model=virtio-net-pci,hostfwd=tcp::"
          guixSSHPort "-:22")
  "-enable-kvm" "-m" guixRAM
  "-device" "virtio-blk,drive=myhd"
  "-drive" (string-append "if=none,file=" guixFile ",id=myhd")
  "-device"
  "virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5"
  "-chardev" "spicevmc,name=vdagent,id=vdagent"
  "-device"
  (string-append
   "virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent"
   ",name=com.redhat.spice.0")
  "-spice" (string-append "port=" guixRemoteViewPort ",disable-ticketing")
  "-vga" "qxl"
  "-smp" cpuCores
  "&"
  ;; "disown" ;; TODO where is disown located???
  ))
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
             (ice-9 popen)
             (ice-9 getopt-long)
             (utils) #| exec |#)

(define vmQcow2File
  (string-append
   "/home/bost" ;; must be my own home dir; '(getenv "HOME")' produces '/root'
   "/virt-machines/"
   #;"guix-system-vm-image-1.3.0.x86_64-linux.qcow2"
   "guix-system-vm-image-1.4.0.x86_64-linux.qcow2"))
(define vmRAM "2G")
(define vmRemoteViewPort "5930")
(define vmSSHPort "10022")
(define (vmCPUCores user)
  (number->string (/ (string->number
                      (exec (list "sudo" (string-append "--user=" user)
                                  "nproc"))) 2)))

#;(format
 #t
 (string-append
  "##### When:\n"
  "#    Could not access KVM kernel module: Permission denied\n"
  "##### do:\n"
  "#    sudo usermod --append --groups kvm $USER\n"
  "# #######################################################\n"
  "\n"
  "# Open a new terminal on the (Ubuntu) host and connect with:\n"
  "#   remote-viewer spice://localhost:~a & disown\n"
  "# or with SSH:\n"
  "#   ssh -o UserKnownHostsFile=/dev/null"
  " -o StrictHostKeyChecking=no -p ~a guest@localhost\n"
  "\n"
  "# Copy file example:\n"
  "#   rsync -avz <srcfile> guest@localhost:</path/to/dstfile>\n")
 vmRemoteViewPort vmSSHPort vmSSHPort)

;; (format #t "\n~a\n\n" (string-join cmd " "))
(define (main args)
 ((compose
   close-pipe
   open-input-pipe
   (lambda (s)
     ;; TODO implement pretty-print for bash commands
     ;; ~a - outputs an argument like display
     ;; ~s - outputs an argument like write (i.e. print to string)
     (format #t "\n~a\n\n" s)
     s)
   (lambda (cmd) (string-join cmd " ")))
  (list
   ;; This works, however without shared clipboard:
   ;; qemu-system-x86_64 \
   ;;     -nic user,model=virtio-net-pci \
   ;;     -enable-kvm -m $vmRAM \
   ;;     -device virtio-blk,drive=myhd \
   ;;     -drive if=none,file=$vmQcow2File,id=myhd \
   ;;     & disown
   ;;
   ;; With shared clipboard and SSH access:
   "qemu-system-x86_64"
   "-nic" (string-append
           "user,model=virtio-net-pci,hostfwd=tcp::"
           vmSSHPort "-:22")
   "-enable-kvm" "-m" vmRAM
   "-device" "virtio-blk,drive=myhd"
   "-drive" (string-append "if=none,file=" vmQcow2File ",id=myhd")
   ;; spice remote-viewer
   #;
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
   "-vga" "qxl"
   "-smp" (vmCPUCores "bost")
   ) " ")
   ;; "&" "disown" ;; TODO where is disown located???
   )))

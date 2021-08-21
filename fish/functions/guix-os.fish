function guix-os --description "GNU Guix via qemu-system-x86_64"
    # see https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

    # Clipboard support:
    # on the (Ubuntu) host run:
    #   sudo apt install gir1.2-spiceclientgtk-3.0 virt-viewer
    # in the VM run:
    #   guix install spice-vdagent virt-viewer
    # I guess the virt-viewer is needed only on the (Ubuntu) host, but let's put
    # it everywhere just in case.

    set guixFile $virtMachines/guix-system-vm-image-1.3.0.x86_64-linux.qcow2
    set guixRAM 2G
    set guixRemoteViewPort 5930
    set guixSSHPort 10022

    # this works, however without shared clipboard:
    # qemu-system-x86_64 \
    #     -nic user,model=virtio-net-pci \
    #     -enable-kvm -m $guixRAM \
    #     -device virtio-blk,drive=myhd \
    #     -drive if=none,file=$guixFile,id=myhd \
    #     & disown

    # with shared clipboard and SSH access:
    qemu-system-x86_64 \
        -nic user,model=virtio-net-pci,hostfwd=tcp::$guixSSHPort-:22 \
        -enable-kvm -m $guixRAM \
        -device virtio-blk,drive=myhd \
        -drive if=none,file=$guixFile,id=myhd \
        -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
        -chardev spicevmc,name=vdagent,id=vdagent \
        -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent,name=com.redhat.spice.0 \
        -spice port=$guixRemoteViewPort,disable-ticketing \
        -vga qxl \
        & disown
    printf "# Open a new terminal on the (Ubuntu) host and connect with:\n"
    printf "    remote-viewer spice://localhost:%s & disown\n" $guixRemoteViewPort
    printf "# or with SSH:\n"
    printf "    ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -p %s guest@localhost\n" $guixSSHPort
    printf "\n"
    printf "# Copy file example:\n"
    printf "    scp -P %s <srcfile> guest@localhost:</path/to/dstfile>\n" $guixSSHPort
end

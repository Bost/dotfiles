function guix-os --description "GNU Guix via qemu-system-x86_64"
    # see https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

    # Clipboard support:
    # on the (Ubuntu) host run:
    #   sudo apt install gir1.2-spiceclientgtk-3.0 virt-viewer
    # in the VM run:
    #   guix install spice-vdagent virt-viewer
    # I guess the virt-viewer is needed only on the (Ubuntu) host, but let's put
    # it everywhere just in case.

    set guixFile ~/guix/guix-system-vm-image-1.3.0.x86_64-linux.qcow2
    set guixRAM 1G

    # 1. this works, however without shared clipboard:
    # qemu-system-x86_64 \
    #     -nic user,model=virtio-net-pci \
    #     -enable-kvm -m $guixRAM \
    #     -device virtio-blk,drive=myhd \
    #     -drive if=none,file=$guixFile,id=myhd \
    #     & disown

    # 2. running with '-spice port=5930,disable-ticketing'...
    qemu-system-x86_64 \
        -nic user,model=virtio-net-pci \
        -enable-kvm -m $guixRAM \
        -device virtio-blk,drive=myhd \
        -drive if=none,file=$guixFile,id=myhd \
        -spice port=5930,disable-ticketing \
        & disown
    # ... starts a qemu process in the background. How to connect to it with
    # virt-viewer is unclear, hmm.

    # 3. trying out the "10.16.2 Using virt-viewer with Spice" instructions...
    # qemu-system-x86_64 \
    #     -nic user,model=virtio-net-pci \
    #     -enable-kvm -m $guixRAM \
    #     -device virtio-blk,drive=myhd \
    #     -drive if=none,file=$guixFile,id=myhd \
    #     -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
    #     -chardev spicevmc,name=vdagent,id=vdagent \
    #     -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent, \
    #     name=com.redhat.spice.0 \
    #     & disown
    # ... produces an error:
    # qemu-system-x86_64: name=com.redhat.spice.0: Could not open 'name=com.redhat.spice.0': No such file or directory

    # 4. omitting the 'name=com.redhat.spice.0'...
    # qemu-system-x86_64 \
    #     -nic user,model=virtio-net-pci \
    #     -enable-kvm -m $guixRAM \
    #     -device virtio-blk,drive=myhd \
    #     -drive if=none,file=$guixFile,id=myhd \
    #     -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
    #     -chardev spicevmc,name=vdagent,id=vdagent \
    #     -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent, \
    #     & disown
    # ... starts Guix in the VM, however without shared clipboard.
end

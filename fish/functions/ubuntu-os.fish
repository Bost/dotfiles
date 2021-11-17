function ubuntu-os --description "Ubuntu via qemu-system-x86_64"
    # see https://nikosmouzakitis.medium.com/running-ubuntu-in-a-virtual-machine-on-qemu-quick-emulator-1607c10f4ba5
    # Create qcow2 file
    # qemu-img create -f qcow2 hda.qcow2 16G

    set isoFile $virtMachines/ubuntu-21.04-desktop-amd64.iso
    set qcow2File $virtMachines/hda.qcow2

    qemu-system-x86_64 \
        -M pc -enable-kvm -cpu host -m 4G \
        -device virtio-net-pci,netdev=net0,romfile="" \
        -netdev type=user,id=net0 \
        -device virtio-blk-pci,drive=drv0 \
        -drive format=qcow2,file=$qcow2File,if=none,id=drv0 \
        -object rng-random,filename=/dev/urandom,id=rng0 \
        -device virtio-rng-pci,rng=rng0 \
        -device virtio-scsi \
        -device scsi-cd,drive=cd \
        -drive if=none,id=cd,file=$isoFile \
        & disown
end
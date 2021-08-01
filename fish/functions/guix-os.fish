function guix-os --description "GNU Guix via qemu-system-x86_64"

set guixFile ~/guix/guix-system-vm-image-1.3.0.x86_64-linux.qcow2
# 6GB RAM
qemu-system-x86_64 \
    -nic user,model=virtio-net-pci \
    -enable-kvm -m 6G \
    -device virtio-blk,drive=myhd \
    -drive if=none,file=$guixFile,id=myhd \
    & disown

# share clipboard using virt-viewer with Spice - doesn't work
# see https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html

# qemu-system-x86_64 \
#     -nic user,model=virtio-net-pci \
#     -enable-kvm -m 6G \
#     -device virtio-blk,drive=myhd \
#     -drive if=none,file=$guixFile,id=myhd \
#     -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
#     -chardev spicevmc,name=vdagent,id=vdagent \
#     -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent, \
#     name=com.redhat.spice.0 \
#     & disown

# qemu-system-x86_64 \
#     -nic user,model=virtio-net-pci \
#     -enable-kvm -m 6G \
#     -device virtio-blk,drive=myhd \
#     -drive if=none,file=$guixFile,id=myhd \
#     -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
#     -chardev spicevmc,name=vdagent,id=vdagent \
#     -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent, \
#     & disown

end

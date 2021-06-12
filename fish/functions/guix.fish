function guix --description "GNU Guix via qemu-system-x86_64"

qemu-system-x86_64 \
    -nic user,model=virtio-net-pci \
    -enable-kvm -m 1024 \
    -device virtio-blk,drive=myhd \
    -drive if=none,file=guix-system-vm-image-1.2.0.x86_64-linux,id=myhd \
    & disown

# share clipboard using virt-viewer with Spice - doesn't work
# see https://guix.gnu.org/manual/en/html_node/Running-Guix-in-a-VM.html
# qemu-system-x86_64 \
#     -nic user,model=virtio-net-pci \
#     -enable-kvm -m 1024 \
#     -device virtio-blk,drive=myhd \
#     -drive if=none,file=guix-system-vm-image-1.2.0.x86_64-linux,id=myhd \
#     -device virtio-serial-pci,id=virtio-serial0,max_ports=16,bus=pci.0,addr=0x5 \
#     -chardev spicevmc,name=vdagent,id=vdagent \
#     -device virtserialport,nr=1,bus=virtio-serial0.0,chardev=vdagent, \
#     name=com.redhat.spice.0 \
#     & disown

end

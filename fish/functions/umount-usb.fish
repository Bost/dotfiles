function umount-usb
    # set cmd umount /sdb1 # TODO verify this
    set cmd udisksctl unmount --block-device=$usbDevice
    echo $cmd
    eval $cmd
end

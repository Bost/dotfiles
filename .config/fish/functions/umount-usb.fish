function umount-usb --description "udisksctl unmount --block-device=$usbDevice"
    # set cmd umount $usbDevice
    # set cmd sudo eject $usbDevice
    set cmd udisksctl unmount --block-device=$usbDevice
    echo $cmd
    eval $cmd
end

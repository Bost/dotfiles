# -*- mode: fish -*-

## fish -n umount-usb.fish
## fish_indent --check umount-usb.fish

function umount-usb --description "udisksctl unmount --block-device=$usbDevice"
    # set cmd umount $usbDevice
    # set cmd sudo eject $usbDevice
    set cmd udisksctl unmount --block-device=$usbDevice
    echo $cmd
    eval $cmd
end

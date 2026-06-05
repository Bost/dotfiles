# -*- mode: fish -*-

## fish -n umount-usb.fish
## fish_indent --check umount-usb.fish

function umount-usb --description "Unmount the USB drive (udisksctl unmount)"
    # trace umount $usbDevice
    # trace sudo eject $usbDevice
    trace udisksctl unmount --block-device=$usbDevice
end

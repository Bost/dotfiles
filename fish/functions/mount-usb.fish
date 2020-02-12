function mount-usb
    # set cmd mount $usbDevice /media/bost/usb-drive
    # set cmd mount --all
    # see also: udisksctl status
    set cmd udisksctl info --block-device $usbDevice "|" grep MountPoints: "|" grep /
    echo $cmd
    eval $cmd
    if test $status = 0
        # set cmd udisksctl unmount --block-device=$usbDevice
        # echo $cmd
        # eval $cmd
    else
        set cmd udisksctl mount --block-device=$usbDevice
        echo $cmd
        eval $cmd
        if test $status = 0
            # echo "Mounted"
        else
            # echo "Mount command failed"
            return $status
        end
    end
end

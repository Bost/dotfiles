function edit-grub
    udiskie-mount /dev/sdb3

    set mountpoint /media/bost/a8fb1680-eef5-49a0-98a3-8169c9b8eeda

    # sudo cp $mountpoint/boot/grub/grub.cfg /tmp/
    # sudo chown bost.users /tmp/grub.cfg
    # chmod +rw /tmp/grub.cfg

    # sudo cp $mountpoint/etc/grub.d/40_custom /tmp/
    # chmod +rw /tmp/40_custom
    # sudo chown bost.users /tmp/40_custom

    ### edit /tmp/grub.cfg /tmp/40_custom /boot/grub/grub.cfg

    sudo cp -i /tmp/grub.cfg $mountpoint/boot/grub/grub.cfg
    sudo cp -i /tmp/40_custom $mountpoint/etc/grub.d/40_custom

    ### reboot and press <F12> to change the boot sequence
end

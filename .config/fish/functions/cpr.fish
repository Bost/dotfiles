# -*- mode: fish -*-

## fish -n cpr.fish
## fish_indent --check cpr.fish

function cpr --description "Copy recursively (cp -r)"
    # TODO cpr doesn't work with sudo:
    #     sudo cpr /boot/grub/ /tmp
    trace cp -r $argv
end

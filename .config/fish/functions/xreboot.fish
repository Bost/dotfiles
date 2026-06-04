# -*- mode: fish -*-

## fish -n xreboot.fish
## fish_indent --check xreboot.fish

function xreboot --description "xfce4-session-logout --reboot --fast"
    # -r, --reboot             Reboot without displaying the logout dialog
    # -f, --fast               Log out quickly; don't save the session
    set cmd xfce4-session-logout --reboot --fast $argv
    echo $cmd
    eval $cmd
end

# -*- mode: fish -*-

## fish -n xreboot.fish
## fish_indent --check xreboot.fish

function xreboot --description "Reboot now, skip dialog (xfce4-session-logout --reboot)"
    # -r, --reboot             Reboot without displaying the logout dialog
    # -f, --fast               Log out quickly; don't save the session
    trace xfce4-session-logout --reboot --fast $argv
end

# -*- mode: fish -*-

## fish -n xshut.fish
## fish_indent --check xshut.fish

function xshut --description "Shut down now, skip dialog (xfce4-session-logout --halt)"
    trace xfce4-session-logout --halt --fast
end

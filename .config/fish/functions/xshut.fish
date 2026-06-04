# -*- mode: fish -*-

## fish -n xshut.fish
## fish_indent --check xshut.fish

function xshut --description "xfce4-session-logout --halt --fast"
    set cmd xfce4-session-logout --halt --fast
    echo $cmd
    eval $cmd
end

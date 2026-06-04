# -*- mode: fish -*-

## fish -n ldir.fish
## fish_indent --check ldir.fish

function ldir --description "ls --color=auto -la -d1 \"*/\" …"
    set cmd ls --color=auto -la -d1 "*/" (string escape -- $argv)
    echo $cmd
    eval $cmd
end

# -*- mode: fish -*-

## fish -n mkcd.fish
## fish_indent --check mkcd.fish

function mkcd --description "Create a directory and cd into it (mkdir; cd)"
    if not test -d $argv
        trace mkdir -p $argv
    end
    if test -d $argv
        cd $argv
    end
end

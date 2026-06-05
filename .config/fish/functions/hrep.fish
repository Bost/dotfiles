# -*- mode: fish -*-

## fish -n hrep.fish
## fish_indent --check hrep.fish

function hrep --description "Search fish history (history --search)"
    set cmd history --show-time="[%Y-%m-%d %H:%M:%S]\ " --reverse --search --contains $argv
    echo $cmd
    eval $cmd
end

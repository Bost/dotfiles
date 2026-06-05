# -*- mode: fish -*-

## fish -n hibernate.fish
## fish_indent --check hibernate.fish

function hibernate --description "Suspend to disk (pm-hibernate)"
    trace sudo pm-hibernate $argv
end

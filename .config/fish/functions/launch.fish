# -*- mode: fish -*-

## fish -n launch.fish
## fish_indent --check launch.fish

# https://github.com/kisaragi-hiu/fish-launch.git
function launch --description "Run a command and forget about it"
    nohup $argv >/dev/null ^/dev/null &
    disown (jobs --last --pid)
end
# TODO complete -c launch -d "Run a command and forget about it." -x -a "(__fish_complete_subcommand_root -u -g)"

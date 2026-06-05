# -*- mode: fish -*-

## fish -n nt.fish
## fish_indent --check nt.fish

function nt --description "Show NetworkManager status (nmcli general status)"
    echo "# NetworkManager"
    trace nmcli general status $argv
end

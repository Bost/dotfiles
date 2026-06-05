# -*- mode: fish -*-

## fish -n nslookup.fish
## fish_indent --check nslookup.fish

function nslookup --description "DNS lookup, replacing nslookup (dig)"
    echo "############################################################"
    echo "### Using dig instead of lookup"
    echo "############################################################"
    trace dig $argv
end

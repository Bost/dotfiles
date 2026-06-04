# -*- mode: fish -*-

## fish -n nslookup.fish
## fish_indent --check nslookup.fish

function nslookup --description "dig …"
    set cmd dig (string escape -- $argv)
    echo $cmd
    echo "############################################################"
    echo "### Using dig instead of lookup"
    echo "############################################################"
    eval $cmd
end

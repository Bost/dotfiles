# -*- mode: fish -*-

## fish -n genpasswd.fish
## fish_indent --check genpasswd.fish

function genpasswd --description "Generate 8 char password"
    echo "To get the password press: C-c"
    # string delims needed (probably) because of the 'echo'
    set cmd "strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo"
    echo $cmd
    eval $cmd
end

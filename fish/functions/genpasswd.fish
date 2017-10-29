function genpasswd
    echo "To get the password press: C-c"
    set cmd "strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo"
    echo $cmd
    eval $cmd
end

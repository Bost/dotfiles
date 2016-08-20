function genpasswd
    echo "To get the password press: C-c"
    echo "strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo"
          strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo
end

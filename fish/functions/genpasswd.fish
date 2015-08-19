function genpasswd
    echo "strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo"
          strings /dev/urandom | grep -o '[[:alnum:]]' | head -n 8 | tr -d '\n'; echo
end

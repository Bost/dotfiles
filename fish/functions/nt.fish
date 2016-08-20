function nt
    echo "NetworkManager:"
    echo "nmcli general status $argv"
          nmcli general status $argv
end

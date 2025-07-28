function nt --description "nmcli general status …"
    echo "# NetworkManager"
    set cmd nmcli general status (string escape -- $argv)
    echo $cmd
    eval $cmd
end

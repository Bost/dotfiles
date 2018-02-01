function telnet
    echo "# 'telnet' is obsolete. Using: 'nc' - arbitrary TCP and UDP connections and listens"
    set cmd nc $argv
    echo $cmd
    eval $cmd
end

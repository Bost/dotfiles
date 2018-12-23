function ldir
    set cmd ls --color=auto -la -d1 "*/" (string escape -- $argv)
    echo $cmd
    eval $cmd
end

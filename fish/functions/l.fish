function l
    set cmd "ls -lA  --color --time-style='+%d-%m-%Y %H:%M:%S' $argv"
    echo $cmd
    eval $cmd
end

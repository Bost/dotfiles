function rg
    set cmd /usr/bin/rg --smart-case (string escape -- $argv)
    echo $cmd
    eval $cmd
end

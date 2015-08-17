function take
    set cmd "mkdir -p $argv; and cd $argv"
    echo $cmd
    eval $cmd
end

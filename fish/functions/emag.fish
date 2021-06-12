function emag --description "ema && glo"
    # this gets immediately evaluated: set cmd ema; and glo
    set cmd cd $dev/emacs
    echo $cmd
    eval $cmd

    set cmd glo
    echo $cmd
    eval $cmd
end

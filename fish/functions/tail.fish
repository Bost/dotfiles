function tail --description "See https://www.brianstorti.com/stop-using-tail/"
    # `tail -f` is better when watching multiple files at the same time
    set less +F $argv
    eval $cmd
    echo $cmd
end

function h
    set escArgv (string escape -- $argv)
    set cmd (printf "history --show-time=\"%s\" %s" $HISTTIMEFORMAT $escArgv)
    echo $cmd
    eval $cmd
end

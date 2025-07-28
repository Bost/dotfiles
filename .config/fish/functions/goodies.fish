function goodies --description "sudo needrestart …"
    # set cmd sudo checkrestart (string escape -- $argv)
    set cmd sudo needrestart (string escape -- $argv)
    echo $cmd
    eval $cmd
end

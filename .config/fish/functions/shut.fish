function shut --description "sudo shutdown …"
    set cmd sudo shutdown (string escape -- $argv)
    echo $cmd
    eval $cmd
end

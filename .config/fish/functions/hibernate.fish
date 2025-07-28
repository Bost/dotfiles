function hibernate --description "sudo pm-hibernate …"
    set cmd sudo pm-hibernate (string escape -- $argv)
    echo $cmd
    eval $cmd
end

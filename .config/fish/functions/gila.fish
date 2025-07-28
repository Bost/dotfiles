function gila --description "git lg-all …"
    set cmd git lg-all (string escape -- $argv)
    echo $cmd
    eval $cmd
end

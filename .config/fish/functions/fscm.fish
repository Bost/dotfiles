function fscm --description "fd --extension scm …"
    set cmd fd --extension scm (string escape -- $argv)
    echo $cmd
    eval $cmd
end

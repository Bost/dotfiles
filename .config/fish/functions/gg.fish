function gg --description "git guix … &"
    set cmd git gui (string escape -- $argv) \&
    echo $cmd
    eval $cmd
end

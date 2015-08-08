function gcom
    set cmd "git checkout master $argv"
    echo $cmd
    eval $cmd    
end

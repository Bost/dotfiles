function glo
    set cmd "git pull --rebase origin $argv"
    echo $cmd
    eval $cmd
    set cmd "git fetch --tags"
    echo $cmd
    eval $cmd
end

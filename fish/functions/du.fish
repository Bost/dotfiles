function du
    set cmd /usr/bin/du (string escape -- $argv)
    echo $cmd
    eval $cmd
    echo "##################"
    echo "### See also ncdu"
end

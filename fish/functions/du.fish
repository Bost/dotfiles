function du
    set cmd /usr/bin/du (string escape -- $argv)
    echo $cmd
    echo "### See also ncdu"
    echo "#################"
    eval $cmd
end

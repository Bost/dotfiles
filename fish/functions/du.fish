function du
    set cmd /usr/bin/du (string escape -- $argv)
    eval $cmd
    echo $cmd "######## See also: ncdu"
end

function dos2unix --description "fromdos …"
    set cmd fromdos (string escape -- $argv)
    echo $cmd
    eval $cmd
end

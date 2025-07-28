function png --description "mtr google.com …"
    set cmd mtr google.com (string escape -- $argv)
    echo $cmd
    eval $cmd
end

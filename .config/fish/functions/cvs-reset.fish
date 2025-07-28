function cvs-reset --description "cvs update -C -l -d -P …"
    set cmd cvs update -C -l -d -P (string escape -- $argv)
    echo $cmd
    eval $cmd
end

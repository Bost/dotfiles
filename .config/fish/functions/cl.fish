function cl --description "xsel --clipboard"
    set cmd xsel --clipboard
    echo $cmd
    eval $cmd
end

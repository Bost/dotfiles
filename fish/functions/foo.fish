function foo -d "Also some code snippets here"
    # if test (string escape -- $argv) = "--switch"
    #     echo '(string escape -- $argv) = "--switch"'
    #     return
    # end

    set cmd cd $dev/foo
    echo $cmd
    eval $cmd
end

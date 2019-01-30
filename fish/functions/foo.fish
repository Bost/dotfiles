function foo -d "Also some code snippets here"
    # examples https://nicolas-van.github.io/programming-with-fish-shell
    math "1 + 2"

    # if test (string escape -- $argv) = "--switch"
    #     echo '(string escape -- $argv) = "--switch"'
    #     return
    # end

    set cmd cd $dev/foo
    echo $cmd
    eval $cmd
end

function time --description="Time just like in Bash"
    # command forces the shell to execute the program COMMANDNAME and ignore any
    # functions or builtins with the same name.
    set cmd command time --portability $argv
    echo $cmd
    eval $cmd
end

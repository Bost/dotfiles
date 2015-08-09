function ga -d "GitUpdateAll"
    # ; and git fetch --tags
    set cmd "cd $dev/cheatsheet; and git pull --rebase origin"
    echo $cmd
    eval $cmd

    # ; and git fetch --tags
    set cmd "cd $dev/dotfiles; and git pull --rebase origin"
    echo $cmd
    eval $cmd
end

function ga -d "GitUpdateAll"
    cd $dev/cheatsheet
    git pull --rebase origin # ; and git fetch --tags

    cd $dev/dotfiles
    git pull --rebase origin # ; and git fetch --tags
end

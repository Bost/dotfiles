function gle
    echo cd ~/dev/emacs-25
         cd ~/dev/emacs-25
    echo git pull --rebase origin $argv
         git pull --rebase origin $argv
    echo git fetch --tags
         git fetch --tags
    echo gitk --all \&
         gitk --all  &
end
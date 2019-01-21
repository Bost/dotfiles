function lg
    # This is in fact: set cmd git lg-all-20 (string escape -- $argv)
    set cmd git log --graph -20 --color --all "--pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)%an%Creset'" (string escape -- $argv)
    echo $cmd
    eval $cmd
end

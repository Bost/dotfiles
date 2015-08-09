function ecg
    set params $argv
    if eval $argv
        set params "."
    end
    
    if pgrep --exact emacs
        set emacsBin emacsclient
    else
        set emacsBin emacs
    end
    set cmd "$emacsBin $dev/cheatsheet/git-commands.js &"
    echo $cmd
    eval $cmd
end

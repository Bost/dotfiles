function ea
    set params $argv
    if eval $argv
        set params "."
    end

    if pgrep --exact emacs
        set emacsBin emacsclient
    else
        set emacsBin emacs
    end
    set cmd "$emacsBin $dev/dotfiles/bash/aliases &"
    echo $cmd
    eval $cmd
end

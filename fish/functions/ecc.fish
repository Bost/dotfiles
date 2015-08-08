function ecc
    set params $argv
    if eval $argv
        set params "."
    end
    
    if pgrep --exact emacs
        set emacsBin emacsclient
    else
        set emacsBin emacs
    end
    eval $emacsBin $dev/cheatsheet/clojure-commands.js &
end

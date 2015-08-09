function e
    set cnt (count $argv)
    if test $cnt = 0
        set params "./"
    else
        set params $argv
    end
    
    if pgrep --exact emacs
        set emacsBin emacsclient
    else
        set emacsBin emacs
    end

    set cmd "$emacsBin $params &"
    echo $cmd
    eval $cmd
end

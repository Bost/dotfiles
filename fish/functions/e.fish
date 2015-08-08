function e
    set cnt (count $argv)
    if test $cnt = 0
        set params "./"
    else
        set params $argv
    end
    
    set pgrepResults pgrep --exact emacs
    set cntResults (count $pgrepResults)
    if test $cntResults = 0
        set emacsBin emacs
    else
        set emacsBin emacsclient
    end
    set cmd "$emacsBin $params &"
    eval $cmd
end

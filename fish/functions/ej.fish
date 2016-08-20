function ej
    set params $argv
    if eval $argv
        set params "."
    end

    set pgrepResults pgrep --exact emacs
    set cntResults (count $pgrepResults)
    if test $cntResults = 0
        set emacsBin emacs
    else
        set emacsBin emacsclient
    end
    set cmd "$emacsBin $dev/dotfiles/jcl/jcl.el &"
    echo $cmd
    eval $cmd
end

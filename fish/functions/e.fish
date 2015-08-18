function e
    set cnt (count $argv)
    if test $cnt = 0
        set params "./"
    else
        set params $argv
    end

    # DYI violation because variables may not be used as commands
    if pgrep --exact emacs
        echo emacsclient $params &
             emacsclient $params &
    else
        echo emacs $params &
             emacs $params &
    end
end

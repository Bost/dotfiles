function ee
    set cnt (count $argv)
    if test $cnt = 0
        set params "./"
    else
        set params $argv
    end

    echo emacs $params &
         emacs $params &
end

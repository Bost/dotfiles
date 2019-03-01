function make-emacs
    set cmd git tag --points-at emacs-26
    echo $cmd
    set list (eval $cmd)
    echo $list
    set cnt (count $list)
    if test $cnt = 0
        set cmd make -j4
        echo $cmd
        eval $cmd
        if test $status = 0
            set cmd sudo make -j4 install
            echo $cmd
            eval $cmd
            if test $status = 0
                set basecmd git describe --abbrev=0 emacs-26 --tags
                # echo "basecmd:" $basecmd
                set taglast (eval $basecmd | grep --only-matching "\([0-9]*\?\)\$")
                # echo "taglast:" $taglast
                set tagbase (eval $basecmd | grep --only-matching "\([0-9]*\?\.[0-9]*\?\.[0-9]*\?\)")
                # echo "tagbase:" $tagbase
                set tagnew (math $taglast + 1)
                # echo "tagnew:" $tagnew
                set cmd git tag $tagbase.$tagnew
                echo $cmd
                eval $cmd
                notify-send "Emacs $tagbase.$tagnew installed"
            end
        end
    else
        echo "Do nothing. Tags on HEAD:" $cnt
    end
end

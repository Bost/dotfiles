function success
    set list (git tag --points-at emacs-26)
    set cnt (count $list)
    if test $cnt = 0
        set basecmd git describe --abbrev=0 emacs-26 --tags
        set taglast ($basecmd | grep --only-matching "\([0-9]*\?\)\$")
        set tagbase ($basecmd | grep --only-matching "\([0-9]*\?\.[0-9]*\?\.[0-9]*\?\)")
        set tagnew (math $taglast + 1)
        set cmd git tag $tagbase.$tagnew
        eval $cmd
        notify-send "Emacs $tagbase.$tagnew installed"
    end
end

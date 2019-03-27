function make-emacs
    # list tags on the HEAD
    set cmd git tag --points-at HEAD
    echo $cmd
    set lstTags (eval $cmd)
    echo $lstTags
    set cntTags (count $lstTags)
    echo "cntTags on HEAD:" $cntTags
    if test $cntTags = 0
        set cmdLastTag git describe --abbrev=0 emacs-26 --tags
        # echo "cmdLastTag:" $cmdLastTag
        set changedFiles (git diff --name-only HEAD..(eval $cmdLastTag))
        # echo $changedFiles
        set lstCodeFiles (codeFiles $changedFiles)
        echo "lstCodeFiles:" $lstCodeFiles
        set cntCodeFiles (count $lstCodeFiles)
        # echo "cntCodeFiles:" $cntCodeFiles
        if test $cntCodeFiles > 0
            set cmd make -j4
            echo $cmd
            eval $cmd
            if test $status = 0
                set cmd sudo make -j4 install
                echo $cmd
                eval $cmd
                if test $status = 0
                    set tagPostfix (eval $cmdLastTag | grep --only-matching "\([0-9]*\?\)\$")
                    # echo "tagPostfix:" $tagPostfix
                    set remoteTag (eval $cmdLastTag | grep --only-matching "\([0-9]*\?\.[0-9]*\?\.[0-9]*\?\)")
                    # echo "remoteTag:" $remoteTag
                    set tagPostfixInc (math $tagPostfix + 1)
                    # echo "tagPostfixInc:" $tagPostfixInc
                    set cmd git tag $remoteTag.$tagPostfixInc
                    echo $cmd
                    eval $cmd
                    notify-send "Emacs $remoteTag.$tagPostfixInc installed"
                end
            end
        end
    else
        echo "Do nothing"
    end
end

function isCodeFile --description "Check if argv has a code-file extention"
    # echo "argv:" $argv
    switch $argv
        case '*.el'
            echo $argv
        case '*.c'
            echo $argv
        case '*.h'
            echo $argv
        case '*.in'
            echo $argv
        case '*.sh'
            echo $argv
        case '*.ac'
            echo $argv
        case '*.guess'
            echo $argv
    end
end

function codeFiles --description "Returns a list of strings (files) with code-file extention"
    # echo "argv:" $argv
    for file in $argv
        # echo "isCodeFile:" (isCodeFile $file)
        if test (isCodeFile $file)
            set lstFiles $lstFiles $file
        end
    end
    echo $lstFiles
end


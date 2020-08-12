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

function make-emacs --description "Compile, install & git-tag emacs src code"
    # ./configure --with-x-toolkit=gtk
    # see also
    # https://github.com/adobe-fonts/source-code-pro/issues/17#issuecomment-487894024
    # list tags on the HEAD
    set cmd git tag --points-at HEAD
    echo $cmd
    set lstTags (eval $cmd)
    echo $lstTags
    set cntTags (count $lstTags)
    echo "cntTags on HEAD:" $cntTags
    if test $cntTags = 0
        # doesn't work - there are no tags on the emacs-27 branch at the moment
        set cmdLastTag git describe --abbrev=0 emacs-27 --tags
        # set cmdLastTag git describe --abbrev=0 emacs-26 --tags
        # echo "cmdLastTag:" $cmdLastTag
        set changedFiles (git diff --name-only HEAD..(eval $cmdLastTag))
        # echo $changedFiles
        set lstCodeFiles (codeFiles $changedFiles)
        echo "lstCodeFiles:" $lstCodeFiles
        set cntCodeFiles (count $lstCodeFiles)
        # echo "cntCodeFiles:" $cntCodeFiles
        if test $cntCodeFiles > 0
            set cntJobs (math (eval nproc) / 2)
            set cmd make --jobs $cntJobs
            echo $cmd
            eval $cmd
            if test $status = 0
                set cmd sudo make --jobs $cntJobs install
                echo $cmd
                eval $cmd
                if test $status = 0
                    set tagPostfix (eval $cmdLastTag | grep --only-matching "\([0-9]*\?\)\$")
                    echo "tagPostfix:" $tagPostfix
                    set remoteTag (eval $cmdLastTag | grep --only-matching "\([0-9]*\?\.[0-9]*\?\.[0-9]*\?\)")
                    echo "remoteTag:" $remoteTag
                    if test $status = 1
                        # TODO parse `emacs --version` to avoid setting remoteTag manually
                        set remoteTag "27.0.60"
                        # set remoteTag "26.2.50"
                        echo "remoteTag not defined. Using:" $remoteTag
                    end
                    set tagPostfixInc (math $tagPostfix + 1)
                    echo "tagPostfixInc:" $tagPostfixInc
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

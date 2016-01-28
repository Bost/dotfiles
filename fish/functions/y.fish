function y --description '$argv must be inside double quotes'
    # Parameter expansion (Globbing) causes trouble
    # set youtube-dl --write-auto-sub --sub-lang 'fr' $argv
    # echo $cmd
    # eval $cmd

    set youtube-dl --write-auto-sub --sub-lang 'fr' \"$argv\"
    echo $cmd
    eval $cmd
end

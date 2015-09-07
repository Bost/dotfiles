function y --description '$argv must be inside double quotes'
    # Parameter expansion (Globbing) causes trouble
    # set youtube-dl --write-auto-sub --sub-lang 'fr' $argv
    # echo $cmd
    # eval $cmd

    echo youtube-dl --write-auto-sub --sub-lang 'fr' $argv
         youtube-dl --write-auto-sub --sub-lang 'fr' $argv
end

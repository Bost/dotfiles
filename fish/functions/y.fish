function y --description '$argv must be inside double quotes'
    echo "youtube-dl --write-auto-sub --sub-lang 'fr' $argv"
          youtube-dl --write-auto-sub --sub-lang 'fr' $argv
    # set cmd youtube-dl --write-auto-sub --sub-lang 'fr' \"$argv\"
    # echo $cmd
    # eval $cmd
end

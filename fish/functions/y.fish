function y --description '$argv must be inside double quotes'
    set cmd youtube-dl --write-auto-sub --sub-lang 'fr' \"$argv\"
    echo $cmd
    eval $cmd
end

function ys
    set cmd youtube-dl --extract-audio --audio-format mp3 $argv
    echo $cmd
    eval $cmd
end

function gco
    # see also abbreviations: abbr --add --global gco git checkout
    set cmd git checkout (string escape -- $argv)
    echo $cmd
    eval $cmd
end

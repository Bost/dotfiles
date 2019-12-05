function f
    # example: f '*.clj'
    # set cmd find -name (string escape -- $argv)
    # -iname case insensitive
    # set cmd find -iname (string escape -- $argv)
    set cmd fdfind $argv
    eval $cmd
    echo "#" $cmd
end

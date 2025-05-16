function hrep --description "Search fish-shell history"
    set cmd history --show-time="[%Y-%m-%d %H:%M:%S]\ " --reverse --search --contains (string escape -- $argv)
    echo $cmd
    eval $cmd
end

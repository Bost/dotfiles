function clojure
    set cmd /usr/local/bin/clojure -A:rebel $argv
    eval $cmd
    echo $cmd
end

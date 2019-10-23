function crc
    # set f1 $dev/cheat/clj/src/clj/cljdocs.clj
    # set f2 $dev/cheat/clj/src/clj/cheat.clj
    # set f3 ~/.lein/profiles.clj
    # set files $f1
    # cheat-grep $argv $files
    # set files $f2 $f3
    # set separator "=============================================="
    # echo $separator $files $separator
    # cheat-grep $argv $files

    set escArgv (string escape -- $argv)
    set base $dev/dotfiles/fish/functions
    set script $base/script.clj
    set deps $base/deps.edn
    # set cmd clojure -Sdeps $deps $script $escArgv "|" grep -Pzie $escArgv
    set cmd clojure $script $escArgv "|" grep -Pzie $escArgv
    echo $cmd
    eval $cmd
end

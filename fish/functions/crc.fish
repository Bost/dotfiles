function crc
    set escArgv (string escape -- $argv)
    # set f1 $dev/cheat/clj/src/clj/cljdocs.clj
    # set f2 $dev/cheat/clj/src/clj/cheat.clj
    # set f3 ~/.lein/profiles.clj
    # set files $f1
    # cheat-grep $escArgv $files
    # set files $f2 $f3
    # set separator "=============================================="
    # echo $separator $files $separator
    # cheat-grep $escArgv $files
    # echo $cmd
    # eval $cmd


    # test if the server is running: nc localhost 5555
    set cmd nmap localhost "|" grep 5555
    echo $cmd
    eval $cmd
    if test $status != 0
        set cmd boot socket-server --port 5555 --accept clojure.core.server/io-prepl wait "&; disown"
        echo $cmd
        eval $cmd
        # set cmd disown
        # echo $cmd
        # eval $cmd
    else
        echo "Already running"
    end
    echo -e "(clojure.core/+ 1 2)\n:repl/quit" | nc localhost 5555

    # set script $dev/dotfiles/fish/functions/script.clj
    # set deps "'{:deps {cheshire {:mvn/version \"5.9.0\"}}}'"
    # set cmd clojure \
    #          -J-Dclojure.server.jvm="'{:port 5555 :accept clojure.core.server/io-prepl}'" \
    #          # -J-Dclojure.server.repl="'{:port 5555 :accept clojure.core.server/repl}'" \
    #          -Sdeps $deps \
    #         #  $script $escArgv \
    #         # "|" grep --color=always -Pzie $escArgv \
    #         # "|" less -r \
    #         ""
    # echo $cmd
    # eval $cmd
end

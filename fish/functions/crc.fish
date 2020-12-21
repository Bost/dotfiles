function crc
    set escArgv (string escape -- $argv)
    set f1 $cheat/clj/src/clj/cljdocs.clj
    set f2 $cheat/clj/src/clj/cheat.clj
    set f3 ~/.lein/profiles.clj
    set f4 ~/.clojure/deps.edn
    set files $f1
    cheat-grep --grep-args="$argv" --files="$files"
    set separator "==============="
    set files $f2 $f3 $f4
    echo $separator $files $separator
    cheat-grep --grep-args="$argv" --files="$files"
    echo $cmd
    eval $cmd

    # set port 5555
    # # test if the server is running: nc localhost 5555
    # set cmd nmap localhost "|" grep $port ">" /dev/null
    # # echo $cmd
    # eval $cmd
    # if test $status != 0
    #     set cmd boot -d cheshire:5.9.0 \
    #         socket-server --port $port \
    #                       # --accept clojure.core.server/io-prepl wait "&;" disown
    #                       --accept clojure.core.server/repl wait "&;" disown
    #     echo $cmd
    #     eval $cmd
    #     set cmd sleep 10
    #     echo $cmd
    #     eval $cmd
    # else
    #     # echo "Port" $port "is open. Assuming the server is running already."
    # end

    # # set script $dev/dotfiles/fish/functions/server.clj
    # # set deps "''"
    # # set cmd clojure \
    # #          -Sdeps $deps \
    # #          $script $escArgv \
    # #         # "|" grep --color=always -Pzie $escArgv \
    # #         # "|" less -r \
    # #         ""
    # # echo $cmd
    # # eval $cmd

    # set script $dev/dotfiles/fish/functions/script.clj
    # # echo -e "(clojure.main/repl :prompt #(printf "my> "))\n(clojure.core/+ 1 2)\n:repl/quit" | nc localhost 5555
    # set cmd nc localhost 5555 "<" $script
    # echo $cmd
    # eval $cmd
    # # set deps "'{:deps {cheshire {:mvn/version \"5.9.0\"}}}'"
    # # set cmd clojure \
    # #          -J-Dclojure.server.jvm="'{:port 5555 :accept clojure.core.server/io-prepl}'" \
    # #          # -J-Dclojure.server.repl="'{:port 5555 :accept clojure.core.server/repl}'" \
    # #          -Sdeps $deps \
    # #         #  $script $escArgv \
    # #         # "|" grep --color=always -Pzie $escArgv \
    # #         # "|" less -r \
    # #         ""
    # # echo $cmd
    # # eval $cmd
end

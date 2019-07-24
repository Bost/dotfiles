function crc
    set f1 $dev/cheat/clj/src/clj/cljdocs.clj
    set f2 $dev/cheat/clj/src/clj/cheat.clj
    set f3 ~/.lein/profiles.clj
    set files $f1
    cheat-grep $argv $files
    set files $f2 $f3
    set separator "=============================================="
    echo $separator $files $separator
    cheat-grep $argv $files
end

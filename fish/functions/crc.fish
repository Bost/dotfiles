function crc
    set f1 $dev/cheatsheet/clj/src/clj/cljdocs.clj
    set f2 $dev/cheatsheet/clj/src/clj/cheat.clj
    set files $f1
    cheat-grep $argv $lispline $files
    set files $f2
    set separator "=============================================="
    echo $separator $files $separator
    cheat-grep $argv $lispline $files
end

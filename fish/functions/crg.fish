function crg
    set f1 $dev/cheatsheet/cmds/git.sh
    set files $f1
    cheat-grep $argv $shellline $files
end

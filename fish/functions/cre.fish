function cre
    set f1 $dev/cheatsheet/cmds/emacs.el
    set files $f1
    cheat-grep $argv $lispline $files
end

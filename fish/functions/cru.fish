function cru
    set f1 $dev/cheatsheet/cmds/utf8.txt
    set files $f1
    grep --ignore-case (string escape -- $argv) $files
end

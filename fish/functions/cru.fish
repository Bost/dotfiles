function cru
    set f1 $dev/cheat/cmds/utf8.txt
    set files $f1
    grep --ignore-case (string escape -- $argv) $files
end

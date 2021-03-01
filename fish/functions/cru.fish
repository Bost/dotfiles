function cru --description="Search for utf8 chars"
    set f1 $cheat/cmds/utf8.txt
    set files $f1
    grep --ignore-case (string escape -- $argv) $files
end

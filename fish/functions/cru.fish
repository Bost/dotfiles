function cru
    set f1 $dev/cheatsheet/cmds/utf8.txt
    set files $f1
    # ack-cheat $files $argv
    # set prm '{:cmt-str "" :files ["'$f1'"]}'
    # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

    grep --ignore-case (string escape -- $argv) $files
end

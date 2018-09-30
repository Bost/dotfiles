function crep
    set f1 $dev/cheatsheet/cmds/linux.sh
    set f2 $dev/cheatsheet/cmds/systemd.sh
    set f3 $dev/cheatsheet/cmds/rest.sh
    set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'"]}'
    lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

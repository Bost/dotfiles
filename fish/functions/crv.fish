function crv
    set f1 $dev/dotfiles/.vimrc
    set prm '{:cmt-str "#" :files ["'$f1'"]}'
    lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

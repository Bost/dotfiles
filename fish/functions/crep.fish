function crep
  # ack-cheat $dev/cheatsheet/cmds/rest.sh $argv
  set f1 $dev/cheatsheet/cmds/linux.sh
  set f2 $dev/cheatsheet/cmds/rest.sh
  set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

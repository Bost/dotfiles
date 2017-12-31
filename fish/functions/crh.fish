function crh
  # ack-cheat $dev/cheatsheet/cmds/host.sh $argv
  set prm '{:cmt-str "#" :files ["'$dev/cheatsheet/cmds/host.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

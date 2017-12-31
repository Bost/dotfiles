function crl
  # ack-cheat $dev/cheatsheet/cmds/linux.sh $argv
  set prm '{:cmt-str "#" :files ["'$dev/cheatsheet/cmds/linux.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

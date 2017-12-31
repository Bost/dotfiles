function crg
  # ack-cheat $dev/cheatsheet/cmds/git.sh $argv
  set prm '{:cmt-str "#" :files ["'$dev/cheatsheet/cmds/git.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

function crl
  # ack-cheat $dev/cheatsheet/cmds/linux.sh $argv
  set files '{:cmt-str "# " :files ["'$dev/cheatsheet/cmds/linux.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

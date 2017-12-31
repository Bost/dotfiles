function cru
  # ack-cheat $dev/cheatsheet/cmds/utf8.txt $argv
  set files '{:cmt-str "" :files ["'$dev/cheatsheet/cmds/utf8.txt'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

function cre
  # ack-cheat $dev/cheatsheet/cmds/emacs.el $argv
  set files '{:cmt-str ";" :files ["'$dev/cheatsheet/cmds/emacs.el'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

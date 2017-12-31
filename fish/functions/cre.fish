function cre
  # ack-cheat $dev/cheatsheet/cmds/emacs.el $argv
  set prm '{:cmt-str ";" :files ["'$dev/cheatsheet/cmds/emacs.el'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

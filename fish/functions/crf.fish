function crf
  # ack-cheat $dev/cheatsheet/cmds/findgrep.sh $argv
  set prm '{:cmt-str "#" :files ["'$dev/cheatsheet/cmds/findgrep.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

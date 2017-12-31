function crw
  # ack-cheat $dev/cheatsheet/cmds/win.bat $argv
  set prm '{:cmt-str "#" :files ["'$dev/cheatsheet/cmds/win.bat'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

  echo "Consider running:"
  echo "    help $argv"
  echo "    $argv /?"
end

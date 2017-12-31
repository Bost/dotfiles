function crc
  # ack-cheat $dev/cheatsheet/cmds/clojure.clj $argv
  set files '{:files ["'$dev/cheatsheet/cmds/linux.sh'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

function crc
  # ack-cheat $dev/cheatsheet/cmds/clojure.clj $argv
  set files '{:cmt-str ";" :files ["'$dev/cheatsheet/cmds/clojure.clj'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

function crc
  # ack-cheat $dev/cheatsheet/cmds/clojure.clj $argv
  set prm '{:cmt-str ";" :files ["'$dev/cheatsheet/clj/src/clj/core.clj'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

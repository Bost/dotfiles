function crc
  # ack-cheat $dev/cheatsheet/cmds/clojure.clj $argv
  set prm '{:cmt-str ";" :files ["'$dev/cheatsheet/cmds/clojure.clj'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

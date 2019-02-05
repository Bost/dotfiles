function cru
  # ack-cheat $dev/cheatsheet/cmds/utf8.txt $argv
  # set prm '{:cmt-str "" :files ["'$dev/cheatsheet/cmds/utf8.txt'"]}'
  # lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

  set f1 $dev/cheatsheet/cmds/utf8.txt
  grep --ignore-case (string escape -- $argv) $f1
end

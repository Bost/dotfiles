function c
  # ack-cheat $dev/cheatsheet/cmds/rest.sh $argv
  set f1 $dev/cheatsheet/cmds/linux.sh
  set f2 $dev/cheatsheet/cmds/rest.sh
  set f3 $dev/cheatsheet/cmds/findgrep.sh
  set f4 $dev/cheatsheet/cmds/git.sh
  set f5 $dev/cheatsheet/cmds/win.bat
  set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'" "'$f3'" "'$f4'" "'$f5'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

  set f1 $dev/cheatsheet/clj/src/clj/core.clj
  set f2 $dev/cheatsheet/cmds/emacs.el
  set prm '{:cmt-str "#" :files ["'$f1'" "'$f2'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv

  set f1 $dev/cheatsheet/cmds/utf8.txt
  set prm '{:cmt-str "" :files ["'$f1'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $prm $argv
end

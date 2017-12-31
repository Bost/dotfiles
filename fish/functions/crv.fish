function crv
  # ack-cheat $dev/cheatsheet/cmds/vim.vim $argv
  set files '{:cmt-str "\"" :files ["'$dev/dotfiles/.vimrc'"]}'
  lumo $dev/dotfiles/lumo/crep.cljs $files $argv
end

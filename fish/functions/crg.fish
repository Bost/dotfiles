function crg
  ack \
  $dev/cheatsheet/cmds/git.sh \
  $dev/dotfiles/.gitconfig \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv
end

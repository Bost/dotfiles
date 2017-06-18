function crep -d "TODO all crep*.fish should use the same code basis"
  # --before-context=1 --after-context=1
  ack \
  $dev/cheatsheet/cmds/git.sh \
  $dev/cheatsheet/cmds/linux.sh \
  $dev/cheatsheet/cmds/win.bat \
  $dev/cheatsheet/cmds/clojure.clj \
  $dev/cheatsheet/cmds/emacs.el \
  $dev/cheatsheet/cmds/utf8.txt \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv

  consider-hint $argv
end

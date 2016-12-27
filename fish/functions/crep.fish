function crep -d "TODO all crep*.fish should use the same code basis"
  # --before-context=1 --after-context=1
  ack-grep \
  $dev/cheatsheet/commands-git.sh \
  $dev/cheatsheet/commands-rest.sh \
  $dev/cheatsheet/commands-clojure.clj \
  $dev/cheatsheet/commands-emacs.el \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv

  consider-hint $argv
end

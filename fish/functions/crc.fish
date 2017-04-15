function crc
  ack \
  $dev/cider/README.md \
  $dev/cheatsheet/commands-clojure.clj \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv
end

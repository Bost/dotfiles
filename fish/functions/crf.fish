function crf
  ack \
  $dev/cheatsheet/commands-findgrep.sh \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv
end

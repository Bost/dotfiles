function crw
  # --before-context=1 --after-context=1
  ack \
  $dev/cheatsheet/commands-win.bat \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match="bold blue" \
  --ignore-case --match $argv

  echo "Consider running:"
  echo "    help $argv"
  echo "    $argv /?"
end

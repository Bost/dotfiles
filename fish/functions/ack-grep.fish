function ack-grep
  # --before-context=1 --after-context=1
  set cmd \
  ack \
  $argv[1] \
  --context=1 \
  --nogroup \
  --break --with-filename --color-filename=grey13 \
  --color-lineno=grey13 --color-match=\"bold blue\" \
  --after-context=1 \
  --ignore-case \
  --match $argv[2..(count $argv)]

  echo $cmd
  eval $cmd

  echo "Consider running:"
  echo "    man -k $argv / apropos -r $argv"
end

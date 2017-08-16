function ack-grep
  set argv_rest $argv[2..(count $argv)]
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
  --match $argv_rest

  echo $cmd
  eval $cmd

  echo "Consider running:"
  echo "    man -k $argv_rest / apropos -r $argv_rest"
end
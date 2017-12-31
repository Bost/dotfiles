function lr
  set cmd "lein repl $argv"
  echo $cmd
  eval $cmd
end

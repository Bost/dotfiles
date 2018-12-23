function lr
  set cmd lein repl (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function lr
  set cmd rlwrap lein repl (string escape -- $argv)
  echo $cmd
  eval $cmd
end

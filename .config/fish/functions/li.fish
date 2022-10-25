function li
  set cmd lein install (string escape -- $argv)
  echo $cmd
  eval $cmd
end

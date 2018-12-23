function lcc
  set cmd lein clean (string escape -- $argv)
  echo $cmd
  eval $cmd
end

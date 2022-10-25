function k9
  set cmd kill -9 (string escape -- $argv)
  echo $cmd
  eval $cmd
end

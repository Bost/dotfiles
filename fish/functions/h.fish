function h
  set cmd history (string escape -- $argv)
  echo $cmd
  eval $cmd
end

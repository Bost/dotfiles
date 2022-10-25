function tf
  set cmd tail -f (string escape -- $argv)
  echo $cmd
  eval $cmd
end

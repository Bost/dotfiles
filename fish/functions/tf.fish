function tf
  set cmd tail -f $argv
  echo $cmd
  eval $cmd
end

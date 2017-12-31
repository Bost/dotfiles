function unexport
  set cmd "unset $argv"
  echo $cmd
  eval $cmd
end

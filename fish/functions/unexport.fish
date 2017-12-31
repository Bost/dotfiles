function unexport
  # TODO test if the string delims are needed?
  set cmd "unset $argv"
  echo $cmd
  eval $cmd
end

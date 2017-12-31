function glh
  set cmd git lg-head $argv
  echo $cmd
  eval $cmd
end

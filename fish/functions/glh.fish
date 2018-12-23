function glh
  set cmd git lg-head (string escape -- $argv)
  echo $cmd
  eval $cmd
end

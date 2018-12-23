function lg
  set cmd git lg-20 (string escape -- $argv)
  echo $cmd
  eval $cmd
end

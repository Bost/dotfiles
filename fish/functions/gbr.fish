function gbr
  set cmd git branch (string escape -- $argv)
  echo $cmd
  eval $cmd
end

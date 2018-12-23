function gbra
  set cmd git branch --all (string escape -- $argv)
  echo $cmd
  eval $cmd
end

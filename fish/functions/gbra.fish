function gbra
  set cmd git branch --all $argv
  echo $cmd
  eval $cmd
end

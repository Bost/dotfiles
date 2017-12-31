function gbra
  set cmd "git branch -a $argv"
  echo $cmd
  eval $cmd
end

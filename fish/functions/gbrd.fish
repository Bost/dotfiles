function gbrd
  set cmd "git branch --delete $argv"
  echo $cmd
  eval $cmd
end

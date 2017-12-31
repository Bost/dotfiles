function gbr
  set cmd git branch $argv
  echo $cmd
  eval $cmd
end

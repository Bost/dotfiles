function grh
  set cmd git reset --hard $argv
  echo $cmd
  eval $cmd
end

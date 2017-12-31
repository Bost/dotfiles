function gap
  set cmd git add --patch $argv
  echo $cmd
  eval $cmd
end

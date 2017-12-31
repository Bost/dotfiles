function glg
  set cmd git log $argv
  echo $cmd
  eval $cmd
end

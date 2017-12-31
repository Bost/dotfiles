function gcov
  set cmd git checkout virtbox $argv
  echo $cmd
  eval $cmd
end

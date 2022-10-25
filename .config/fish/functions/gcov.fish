function gcov
  set cmd git checkout virtbox (string escape -- $argv)
  echo $cmd
  eval $cmd
end

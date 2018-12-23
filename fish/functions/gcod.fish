function gcod
  set cmd git checkout - (string escape -- $argv)
  echo $cmd
  eval $cmd
end

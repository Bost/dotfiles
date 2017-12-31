function gcod
  set cmd "git checkout - $argv"
  echo $cmd
  eval $cmd
end

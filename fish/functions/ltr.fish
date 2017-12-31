function ltr
  set cmd "ls -latr $argv"
  echo $cmd
  eval $cmd
end

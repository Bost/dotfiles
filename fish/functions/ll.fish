function ll
  set cmd "ls -lh $argv"
  echo $cmd
  eval $cmd
end

function ll
  set cmd ls -lh (string escape -- $argv)
  echo $cmd
  eval $cmd
end

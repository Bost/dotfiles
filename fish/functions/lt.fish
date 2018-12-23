function lt
  set cmd ls -lat (string escape -- $argv)
  echo $cmd
  eval $cmd
end

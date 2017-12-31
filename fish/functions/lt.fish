function lt
  set cmd ls -lat $argv
  echo $cmd
  eval $cmd
end

function klt
  set cmd kill -9 ltbin (string escape -- $argv)
  echo $cmd
  eval $cmd
end

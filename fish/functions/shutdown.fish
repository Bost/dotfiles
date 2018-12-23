function shutdown
  set cmd sudo shutdown -h now (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function shutdown
  set cmd sudo shutdown -h now $argv
  echo $cmd
  eval $cmd
end

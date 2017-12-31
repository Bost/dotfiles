function hibernate
  set cmd sudo pm-hibernate $argv
  echo $cmd
  eval $cmd
end

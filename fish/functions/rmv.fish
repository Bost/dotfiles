function rmv
  set cmd sudo apt remove --yes (string escape -- $argv)
  echo $cmd
  eval $cmd
end

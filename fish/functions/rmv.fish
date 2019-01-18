function rmv
  set cmd sudo snap remove (string escape -- $argv)
  # set cmd sudo apt remove --yes (string escape -- $argv)
  echo $cmd
  eval $cmd
end

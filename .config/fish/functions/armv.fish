function armv
  set cmd sudo apt autoremove --yes (string escape -- $argv)
  echo $cmd
  eval $cmd
end

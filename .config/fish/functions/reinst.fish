function reinst
  set cmd sudo apt --reinstall install --yes (string escape -- $argv)
  echo $cmd
  eval $cmd
end

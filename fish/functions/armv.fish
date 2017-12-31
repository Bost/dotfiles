function armv
  set cmd sudo apt autoremove --yes $argv
  echo $cmd
  eval $cmd
end

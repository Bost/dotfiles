function purge
  set cmd sudo apt purge --yes $argv
  echo $cmd
  eval $cmd
end

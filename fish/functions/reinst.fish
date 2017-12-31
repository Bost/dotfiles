function reinst
  set cmd "sudo apt --reinstall install --yes $argv"
  echo $cmd
  eval $cmd
end

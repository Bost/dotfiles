function rmv
  set cmd "sudo apt remove --yes $argv"
  echo $cmd
  eval $cmd
end

function inst
  set cmd "sudo apt install --yes $argv"
  echo $cmd
  eval $cmd
end

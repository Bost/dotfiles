function gcl
  set cmd "git clone $argv"
  echo $cmd
  eval $cmd
end

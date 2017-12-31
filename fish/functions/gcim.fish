function gcim
  set cmd "git commit -m $argv"
  echo $cmd
  eval $cmd
end

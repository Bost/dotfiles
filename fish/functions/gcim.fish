function gcim
  set cmd git commit -m (string escape -- $argv)
  echo $cmd
  eval $cmd
end

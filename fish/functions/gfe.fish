function gfe
  set cmd git fetch (string escape -- $argv)
  echo $cmd
  eval $cmd
end

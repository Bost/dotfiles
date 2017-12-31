function gfe
  set cmd git fetch $argv
  echo $cmd
  eval $cmd
end

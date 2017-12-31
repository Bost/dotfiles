function gad
  set cmd git add $argv
  echo $cmd
  eval $cmd
end

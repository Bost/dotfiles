function ghof
  set cmd git push origin --force $argv
  echo $cmd
  eval $cmd
end

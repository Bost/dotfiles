function ghof
  set cmd git push origin --force (string escape -- $argv)
  echo $cmd
  eval $cmd
end

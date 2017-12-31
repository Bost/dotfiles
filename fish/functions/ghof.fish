function ghof
  set cmd "git push origin -f $argv"
  echo $cmd
  eval $cmd
end

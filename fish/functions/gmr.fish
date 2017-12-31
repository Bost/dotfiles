function gmr
  set cmd git merge $argv
  echo $cmd
  eval $cmd
end

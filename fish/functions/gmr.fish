function gmr
  set cmd git merge (string escape -- $argv)
  echo $cmd
  eval $cmd
end

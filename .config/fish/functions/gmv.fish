function gmv
  set cmd git mv (string escape -- $argv)
  echo $cmd
  eval $cmd
end

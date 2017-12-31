function gmv
  set cmd git mv $argv
  echo $cmd
  eval $cmd
end

function g
  set cmd egrep -i $argv
  echo $cmd
  eval $cmd
end

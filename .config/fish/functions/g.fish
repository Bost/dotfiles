function g
  set cmd egrep -i (string escape -- $argv)
  echo $cmd
  eval $cmd
end

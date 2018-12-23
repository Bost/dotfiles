function goodies
  set cmd sudo checkrestart (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function d2u
  set cmd fromdos (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function dos2unix
  set cmd fromdos (string escape -- $argv)
  echo $cmd
  eval $cmd
end

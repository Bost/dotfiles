function dos2unix
  set cmd "fromdos $argv"
  echo $cmd
  eval $cmd
end

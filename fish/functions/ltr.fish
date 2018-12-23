function ltr
  set cmd ls -latr (string escape -- $argv)
  echo $cmd
  eval $cmd
end

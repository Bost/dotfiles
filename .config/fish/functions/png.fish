function png
  set cmd mtr google.com (string escape -- $argv)
  echo $cmd
  eval $cmd
end

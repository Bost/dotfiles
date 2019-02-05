function cx
  set cmd chmod +x (string escape -- $argv)
  echo $cmd
  eval $cmd
end

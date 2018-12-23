function cw
  set cmd chmod +w (string escape -- $argv)
  echo $cmd
  eval $cmd
end

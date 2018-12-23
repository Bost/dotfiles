function gco
  set cmd git checkout (string escape -- $argv)
  echo $cmd
  eval $cmd
end

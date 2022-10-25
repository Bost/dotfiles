function gfeu
  set cmd git fetch upstream (string escape -- $argv)
  echo $cmd
  eval $cmd
end

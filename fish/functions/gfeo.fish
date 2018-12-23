function gfeo
  set cmd git fetch origin (string escape -- $argv)
  echo $cmd
  eval $cmd
end

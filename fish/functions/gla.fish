function gla
  set cmd git lg-all (string escape -- $argv)
  echo $cmd
  eval $cmd
end

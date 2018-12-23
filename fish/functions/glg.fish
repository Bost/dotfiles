function glg
  set cmd git log (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function gad
  set cmd git add (string escape -- $argv)
  echo $cmd
  eval $cmd
end

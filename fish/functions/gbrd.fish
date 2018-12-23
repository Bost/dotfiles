function gbrd
  set cmd git branch --delete (string escape -- $argv)
  echo $cmd
  eval $cmd
end

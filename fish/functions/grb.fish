function grb
  set cmd git rebase (string escape -- $argv)
  echo $cmd
  eval $cmd
end

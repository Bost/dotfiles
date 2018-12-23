function gra
  set cmd git rebase --abort (string escape -- $argv)
  echo $cmd
  eval $cmd
end

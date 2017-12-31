function gra
  set cmd "git rebase --abort $argv"
  echo $cmd
  eval $cmd
end

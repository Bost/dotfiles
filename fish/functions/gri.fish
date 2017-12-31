function gri
  set cmd "git rebase --interactive $argv"
  echo $cmd
  eval $cmd
end

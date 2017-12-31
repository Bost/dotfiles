function grb
  set cmd "git rebase $argv"
  echo $cmd
  eval $cmd
end

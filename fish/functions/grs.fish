function grs
  set cmd "git rebase --skip $argv"
  echo $cmd
  eval $cmd
end

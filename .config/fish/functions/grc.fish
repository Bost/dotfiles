function grc
  set cmd git rebase --continue (string escape -- $argv)
  echo $cmd
  eval $cmd
end

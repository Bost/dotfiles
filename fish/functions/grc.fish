function grc
  set cmd git rebase --continue $argv
  echo $cmd
  eval $cmd
end

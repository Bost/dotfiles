function gri --description "git rebase --interactive"
  set cmd git rebase --interactive (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function gcianoe
  set cmd git commit --amend --no-edit (string escape -- $argv)
  echo $cmd
  eval $cmd
end

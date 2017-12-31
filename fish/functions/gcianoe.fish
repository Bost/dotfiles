function gcianoe
  set cmd git commit --amend --no-edit $argv
  echo $cmd
  eval $cmd
end

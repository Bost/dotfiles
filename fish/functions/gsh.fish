function gsh
  set cmd "git stash save $argv"
  echo $cmd
  eval $cmd
end

function gsh
  set cmd git stash save (string escape -- $argv)
  echo $cmd
  eval $cmd
end

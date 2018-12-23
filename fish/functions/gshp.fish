function gshp
  set cmd git stash pop (string escape -- $argv)
  echo $cmd
  eval $cmd
end

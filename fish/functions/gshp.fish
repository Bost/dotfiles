function gshp
  set cmd "git stash pop $argv"
  echo $cmd
  eval $cmd
end

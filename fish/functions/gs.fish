function gs
  set cmd git status --short --branch $argv
  echo $cmd
  eval $cmd
end

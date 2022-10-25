function gs
  set cmd git status --short --branch (string escape -- $argv)
  echo $cmd
  eval $cmd
end

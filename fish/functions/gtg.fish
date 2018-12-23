function gtg
  set cmd git tag --sort version:refname (string escape -- $argv)
  echo $cmd
  eval $cmd
end

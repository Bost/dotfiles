function gtg
  set cmd git tag --sort version:refname $argv
  echo $cmd
  eval $cmd
end

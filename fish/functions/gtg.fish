function gtg
  set cmd git tag --sort version:refname (string escape -- $argv)
  echo $cmd
  eval $cmd
  # repeat the command in case of too many tags to were printed
  echo "# Command was:" $cmd
end

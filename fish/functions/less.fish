function less
  set cmd (which less) "-r $argv"
  echo $cmd
  eval $cmd
end

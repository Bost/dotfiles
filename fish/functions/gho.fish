function gho
  set cmd git push --verbose origin (string escape -- $argv)
  # set cmd git push --verbose origin master:refs/heads/master
  echo $cmd
  eval $cmd
end

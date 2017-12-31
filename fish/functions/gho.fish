function gho
  set cmd "git push -v origin master:refs/heads/master"
  echo $cmd
  eval $cmd
end

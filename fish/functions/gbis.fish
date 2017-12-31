function gbis
  set cmd "git bisect start $argv"
  echo $cmd
  eval $cmd
end

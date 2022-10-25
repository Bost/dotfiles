function gbis
  set cmd git bisect start (string escape -- $argv)
  echo $cmd
  eval $cmd
end

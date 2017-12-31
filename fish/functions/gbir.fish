function gbir
  set cmd "git bisect reset $argv"
  echo $cmd
  eval $cmd
end

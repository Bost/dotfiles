function gbir
  set cmd git bisect reset (string escape -- $argv)
  echo $cmd
  eval $cmd
end

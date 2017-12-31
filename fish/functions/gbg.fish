function gbg
  set cmd git bisect good $argv
  echo $cmd
  eval $cmd
end

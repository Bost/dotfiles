function gbg --description "git bisect good"
  set cmd git bisect good (string escape -- $argv)
  echo $cmd
  eval $cmd
end

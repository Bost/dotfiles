function lf
  set cmd lein figwheel (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function lf
  set cmd lein figwheel $argv
  echo $cmd
  eval $cmd
end

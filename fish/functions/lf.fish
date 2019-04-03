function lf
  set cmd rlwrap lein figwheel (string escape -- $argv)
  echo $cmd
  eval $cmd
end

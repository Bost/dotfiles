function lco
  set cmd lein cljsbuild once (string escape -- $argv)
  echo $cmd
  eval $cmd
end

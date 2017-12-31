function lco
  set cmd lein cljsbuild once $argv
  echo $cmd
  eval $cmd
end

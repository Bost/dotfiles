function lc
  set cmd lein clean $argv
  echo $cmd
  eval $cmd
end

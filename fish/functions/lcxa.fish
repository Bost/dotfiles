function lcxa
  set cmd lein cljx auto (string escape -- $argv)
  echo $cmd
  eval $cmd
end

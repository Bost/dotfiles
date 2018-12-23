function lcad
  set cmd lein cljsbuild auto dev (string escape -- $argv)
  echo $cmd
  eval $cmd
end

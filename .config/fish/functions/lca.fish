function lca
  set cmd lein cljsbuild auto (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function lca
  set cmd "lein cljsbuild auto $argv"
  echo $cmd
  eval $cmd
end

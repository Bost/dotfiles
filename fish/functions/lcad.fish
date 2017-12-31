function lcad
  set cmd "lein cljsbuild auto dev $argv"
  echo $cmd
  eval $cmd
end

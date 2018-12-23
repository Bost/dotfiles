function gdf
  set cmd git diff --word-diff (string escape -- $argv)
  echo $cmd
  eval $cmd
end

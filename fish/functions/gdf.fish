function gdf
  set cmd "git diff --word-diff $argv"
  echo $cmd
  eval $cmd
end

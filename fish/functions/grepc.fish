function grepc
  # string delims needed - the $argv is in the middle
  set cmd "grep -nir $argv --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./"
  echo $cmd
  eval $cmd
end

function cg
  # string delims needed - because of the $argv in the middle
  set cmd "grep -nir \"$argv\" --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./"
  echo $cmd
  eval $cmd
end

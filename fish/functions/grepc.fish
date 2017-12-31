function grepc
  set cmd "grep -nir $argv --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./"
  echo $cmd
  eval $cmd
end

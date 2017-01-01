function cg
  echo "grep -nir \"$argv\" --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./"
        grep -nir  "$argv"  --exclude-dir={.git,CVS} --include=\*.{el,clj,cljs,cljc} ./
end

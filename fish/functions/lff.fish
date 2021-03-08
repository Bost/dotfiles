function lff --description="l with full paths"
  set cmd ls -lrt -d -1 $PWD/{*,.*}
  echo $cmd
  eval $cmd
end

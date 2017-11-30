function cvs-reset
  set cmd "cvs update -C -l -d -P $argv"
  echo $cmd
  eval $cmd
end

function cvs-reset
  set cmd cvs update -C -l -d -P (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function cp
  set cmd /bin/cp (string escape -- $argv)
  echo $cmd
  echo "######## See also: rsync -azv (string escape -- $argv)"
  eval $cmd
end

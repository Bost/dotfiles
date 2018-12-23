function cp
  set cmd /bin/cp (string escape -- $argv)
  echo $cmd
  eval $cmd
  echo "# Try out: rsync -azv (string escape -- $argv)"
end

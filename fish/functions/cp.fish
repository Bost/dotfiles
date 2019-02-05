function cp
  set cmd /bin/cp (string escape -- $argv)
  echo $cmd
  echo "# Try out: rsync -azv (string escape -- $argv)"
  echo "##############################################"
  eval $cmd
end

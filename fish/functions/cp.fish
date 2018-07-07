function cp
  set cmd "/bin/cp $argv"
  echo $cmd
  eval $cmd
  echo "# Try out: rsync -azv $argv"
end

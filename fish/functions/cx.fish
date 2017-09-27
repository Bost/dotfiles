function cx
  echo "# chmod a+rx $argv"
  set cmd "chmod +x $argv"
  echo $cmd
  eval $cmd
end

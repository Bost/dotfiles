function cx
  set cmd chmod +x (string escape -- $argv)
  echo $cmd
  eval $cmd
  echo "### Try alternative"
  echo "# chmod a+rx (string escape -- $argv)"
end

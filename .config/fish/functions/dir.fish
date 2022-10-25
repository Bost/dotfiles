function dir
  set cmd ls --color=auto --format=vertical (string escape -- $argv)
  echo $cmd
  eval $cmd
end

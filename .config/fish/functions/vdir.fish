function vdir
  set cmd ls --color=auto --format=long (string escape -- $argv)
  echo $cmd
  eval $cmd
end

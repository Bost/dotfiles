function dir
  set cmd ls --color=auto --format=vertical $argv
  echo $cmd
  eval $cmd
end

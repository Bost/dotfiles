function vdir
  set cmd "ls --color=auto --format=long $argv"
  echo $cmd
  eval $cmd
end

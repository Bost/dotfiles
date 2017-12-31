function lh
  set cmd "ls -lAh --color --time-style '+%d-%m-%Y %H:%M:%S' $argv"
  echo $cmd
  eval $cmd
end

function hN0
  set cmd "history | tail $argv"
  echo $cmd
  eval $cmd
end

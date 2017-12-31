function wipe
  set cmd "printf '\ec' $argv"
  echo $cmd
  eval $cmd
end

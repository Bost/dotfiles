function gk
  set cmd "gitk --all $argv &"
  echo $cmd
  eval $cmd
end

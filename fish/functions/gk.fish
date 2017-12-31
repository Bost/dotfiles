function gk
  # string delims needed because of the '&'
  set cmd "gitk --all $argv &"
  echo $cmd
  eval $cmd
end

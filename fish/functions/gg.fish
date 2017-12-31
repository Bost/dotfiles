function gg
  # string delims needed because of the '&'
  set cmd "git gui $argv &"
  echo $cmd
  eval $cmd
end

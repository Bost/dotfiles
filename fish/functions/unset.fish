function unset
  # TODO test if the string delims are needed?
  set cmd "set --erase $argv"
  echo $cmd
  eval $cmd
end

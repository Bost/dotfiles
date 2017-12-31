function unset
  set cmd "set --erase $argv"
  echo $cmd
  eval $cmd
end

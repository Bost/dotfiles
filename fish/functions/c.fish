function c
  set cmd cat $argv
  # echo $cmd # otherwise c <file> | jq '.' doesn't work
  eval $cmd
end

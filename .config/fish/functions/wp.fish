function wp --description "printf '\ec' : wipe screen"
  # See also `reset`
  set cmd printf '\ec' (string escape -- $argv)
  eval $cmd
  echo $cmd
end

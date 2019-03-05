function wp --description "wipe screen"
  set cmd printf '\ec' (string escape -- $argv)
  eval $cmd
  echo $cmd
end

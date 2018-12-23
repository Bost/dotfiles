function wipe
  set cmd printf '\ec' (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function unset
  set cmd set --erase (string escape -- $argv)
  echo $cmd
  eval $cmd
end

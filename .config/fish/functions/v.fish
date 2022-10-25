function v
  set cmd vim (string escape -- $argv)
  echo $cmd
  eval $cmd
end

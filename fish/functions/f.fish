function f
  set cmd find . -name (string escape -- $argv)
  echo $cmd
  eval $cmd
end

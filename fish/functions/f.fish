function f
  set cmd find . -name $argv
  echo $cmd
  eval $cmd
end

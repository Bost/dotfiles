function hrep
  set cmd history --search --contains (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function hrep
  set cmd history --search --contains $argv
  echo $cmd
  eval $cmd
end

function li
  set cmd "lein install $argv"
  echo $cmd
  eval $cmd
end

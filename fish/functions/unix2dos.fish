function unix2dos
  set cmd todos (string escape -- $argv)
  echo $cmd
  eval $cmd
end

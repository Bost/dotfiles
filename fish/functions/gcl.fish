function gcl
  set cmd git clone (string escape -- $argv)
  echo $cmd
  eval $cmd
end

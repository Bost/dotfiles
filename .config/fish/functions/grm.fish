function grm
  set cmd git rm (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function grh
  set cmd git reset --hard (string escape -- $argv)
  echo $cmd
  eval $cmd
end

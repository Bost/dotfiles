function gh
  set cmd git push (string escape -- $argv)
  echo $cmd
  eval $cmd
end

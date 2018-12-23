function gap
  set cmd git add --patch (string escape -- $argv)
  echo $cmd
  eval $cmd
end

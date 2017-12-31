function gh
  set cmd git push $argv
  echo $cmd
  eval $cmd
end

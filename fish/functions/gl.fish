function gl
  set cmd git pull $argv
  echo $cmd
  eval $cmd
end

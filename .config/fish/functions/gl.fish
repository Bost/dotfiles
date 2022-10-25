function gl
  set cmd git pull (string escape -- $argv)
  echo $cmd
  eval $cmd
end

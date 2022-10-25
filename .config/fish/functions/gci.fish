function gci
  set cmd git commit (string escape -- $argv)
  echo $cmd
  eval $cmd
end

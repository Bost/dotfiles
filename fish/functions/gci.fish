function gci
  set cmd git commit $argv
  echo $cmd
  eval $cmd
end

function gg
  set cmd git gui (string escape -- $argv) \&
  echo $cmd
  eval $cmd
end

function gk
  set cmd gitk --all (string escape -- $argv) \&
  echo $cmd
  eval $cmd
end

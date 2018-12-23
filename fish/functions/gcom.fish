function gcom
  set cmd git checkout master (string escape -- $argv)
  echo $cmd
  eval $cmd
end

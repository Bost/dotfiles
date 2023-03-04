function gbra --description "git branch --all"
  set cmd git branch --all (string escape -- $argv)
  echo $cmd
  eval $cmd
end

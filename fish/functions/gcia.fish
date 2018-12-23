function gcia
  set cmd git commit --amend (string escape -- $argv)
  echo $cmd
  eval $cmd
end

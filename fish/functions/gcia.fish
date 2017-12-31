function gcia
  set cmd git commit --amend $argv
  echo $cmd
  eval $cmd
end

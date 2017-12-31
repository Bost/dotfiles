function gfeu
  set cmd "git fetch upstream $argv"
  echo $cmd
  eval $cmd
end

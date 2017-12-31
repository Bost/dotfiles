function gbrm
  set cmd "git branch -m $argv"
  echo $cmd
  eval $cmd
end

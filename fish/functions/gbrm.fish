function gbrm
  set cmd git branch --move $argv
  echo $cmd
  eval $cmd
end

function gbrm
  set cmd git branch --move (string escape -- $argv)
  echo $cmd
  eval $cmd
end

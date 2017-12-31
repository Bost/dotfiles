function ghe
  set cmd git help $argv
  echo $cmd
  eval $cmd
end

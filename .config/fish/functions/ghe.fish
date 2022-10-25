function ghe
  set cmd git help (string escape -- $argv)
  echo $cmd
  eval $cmd
end

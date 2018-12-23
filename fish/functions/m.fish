function m
  set cmd mplayer (string escape -- $argv)
  echo $cmd
  eval $cmd
end

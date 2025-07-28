function m --description "mplayer …"
  set cmd mplayer (string escape -- $argv)
  echo $cmd
  eval $cmd
end

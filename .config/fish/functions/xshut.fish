function xshut --description "xfce4-session-logout --halt --fast"
  set cmd xfce4-session-logout --halt --fast
  echo $cmd
  eval $cmd
end

function loff
  set cmd xfce4-session-logout --logout --fast
  echo $cmd
  eval $cmd
end

function xreboot --description "xfce4-session-logout --reboot --fast"
  set cmd xfce4-session-logout --reboot --fast
  echo $cmd
  eval $cmd
end

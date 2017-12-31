function reboot
  set cmd xfce4-session-logout --reboot --fast
  echo $cmd
  eval $cmd
end

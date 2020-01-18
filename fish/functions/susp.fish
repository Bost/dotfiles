function susp
  # set cmd xfce4-session-logout --suspend
  set cmd systemctl suspend
  echo $cmd
  eval $cmd
end

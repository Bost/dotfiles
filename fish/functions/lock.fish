function lock
  # gnome-screensaver-command -l
  set cmd xflock4
  echo $cmd
  eval $cmd
end

function lock
  # TODO check if xfce4-screensaver is installed. Otherwise
  # sudo apt install xfce4-screensaver
  # gnome-screensaver-command -l
  set cmd xflock4
  echo $cmd
  eval $cmd
end

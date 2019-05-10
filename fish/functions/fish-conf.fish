function fish-conf
  set cmd cd ~/.config/fish
  echo $cmd
  eval $cmd
  echo "Reload config:"
  echo "    source ~/.config/fish"
end

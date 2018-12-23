function update-grub
  set cmd sudo update-grub (string escape -- $argv)
  echo $cmd
  eval $cmd
end

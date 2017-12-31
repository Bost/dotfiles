function update-grub
  set cmd "sudo update-grub $argv"
  echo $cmd
  eval $cmd
end

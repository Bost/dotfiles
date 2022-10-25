function nt
  echo "# NetworkManager"
  set cmd nmcli general status (string escape -- $argv)
  echo $cmd
  eval $cmd
end

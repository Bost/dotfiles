function nt
  echo "NetworkManager:"
  set cmd "nmcli general status $argv"
  echo $cmd
  eval $cmd
end

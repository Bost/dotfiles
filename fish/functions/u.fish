function u
  # string delims - '; and'
  set cmd "sudo apt update; and sudo apt full-upgrade --yes; and sudo checkrestart"
  echo $cmd
  eval $cmd
  echo "# Try out: sudo apt update; and sudo apt upgrade; and sudo apt dist-upgrade"
  echo "# Try out: sudo rm /var/lib/update-manager/meta-release-lts"
end

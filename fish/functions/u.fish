function u
  # string delims - '; and'
  set cmd "sudo apt update; and sudo apt full-upgrade --yes; and sudo checkrestart"
  # TODO do not execute checkrestart when nothing upgraded
  # sudo apt full-upgrade --yes | tail -1 | grep --only-matching "\([0-9]*\?\)"
  echo $cmd
  eval $cmd
  echo "# Try out: sudo apt update; and sudo apt upgrade; and sudo apt dist-upgrade"
  echo "# Try out: sudo rm /var/lib/update-manager/meta-release-lts"
end

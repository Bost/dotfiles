function ifconfig
  echo "######################################################"
  echo "### 'ifconfig' is obsolete. Using: 'ip address'    ###"
  echo "######################################################"
  set ip addr show $argv
  echo $cmd
  eval $cmd
end

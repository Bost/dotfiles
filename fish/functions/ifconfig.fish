function ifconfig
  echo "######################################################"
  echo "### 'ifconfig' is obsolete. Using: 'ip address'    ###"
  echo "######################################################"
  echo ip addr show $argv
       ip addr show $argv
end

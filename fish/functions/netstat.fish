function netstat
  echo "# 'netstat' is obsolete. Using: 'ss' - socket statistics"
  set cmd "ss $argv"
  echo $cmd
  eval $cmd
end

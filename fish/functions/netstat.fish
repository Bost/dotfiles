function netstat
  echo "# 'netstat' is obsolete. Using: 'ss' - socket statistics / investigation"
  set cmd ss $argv
  echo $cmd
  eval $cmd
end

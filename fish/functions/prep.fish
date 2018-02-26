function prep
  set cmd pgrep --list-full $argv
  echo $cmd
  eval $cmd
  echo "### WARN pgrep doesn't list full command line; use: ps aux | grep <str>"
end

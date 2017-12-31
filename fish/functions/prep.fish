function prep
  set cmd "pgrep --list-full $argv"
  echo $cmd
  eval $cmd
end

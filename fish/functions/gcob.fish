function gcob
  set cmd "git checkout -b $argv"
  echo $cmd
  eval $cmd
end

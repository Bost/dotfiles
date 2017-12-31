function grm
  set cmd "git rm $argv"
  echo $cmd
  eval $cmd
end

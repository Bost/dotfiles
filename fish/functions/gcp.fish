function gcp
  set cmd "git cherry-pick $argv"
  echo $cmd
  eval $cmd
end

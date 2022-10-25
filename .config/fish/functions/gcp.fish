function gcp
  set cmd git cherry-pick (string escape -- $argv)
  echo $cmd
  eval $cmd
end

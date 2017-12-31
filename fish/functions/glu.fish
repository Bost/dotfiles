function glu
  set cmd "git pull --rebase upstream master; and git fetch --tags upstream"
  echo $cmd
  eval $cmd
end

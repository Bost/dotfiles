function glu
  # string delims needed because of the '; and'
  set cmd "git pull --rebase upstream master; and git fetch --tags upstream"
  echo $cmd
  eval $cmd
end

function u
  set cmd "sudo apt update; and sudo apt full-upgrade --yes; and sudo checkrestart"
  echo $cmd
  eval $cmd
end

function envp
  set cmd "env | grep '^PATH=.*'"
  echo $cmd
  eval $cmd
end

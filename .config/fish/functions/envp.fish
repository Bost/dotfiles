function envp
  # string delims needed because of the '|'
  set cmd "env | grep '^PATH=.*'"
  echo $cmd
  eval $cmd
end

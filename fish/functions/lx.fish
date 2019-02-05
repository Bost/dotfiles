function lx
  # find . -maxdepth 1 -perm -111 -type f
  set cmd find . -maxdepth 1 -perm -111 -type f
  echo $cmd
  eval $cmd
end

function lx
  # echo "find . -maxdepth 1 -perm -111 -type f"
        # find . -maxdepth 1 -perm -111 -type f
  set cmd find . -maxdepth 1 -perm -111 -type f
  echo $cmd
  eval $cmd
  # eval cmd
  # echo "ls -la $results $argv"
  #       ls -la $results
end

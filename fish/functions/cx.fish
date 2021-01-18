function cx
  printf "# Example:\n"
  printf "#    chmod --recursive u=rwx,g=rwx,o=rwx /path/to/dir"
  set cmd chmod +x (string escape -- $argv)
  echo $cmd
  eval $cmd
end

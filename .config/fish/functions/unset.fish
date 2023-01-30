function unset
  set cmd (printf "set --erase %s" (string escape -- $argv))
  printf "ERR: command doesn't work '%s'\n" $cmd
  return 1 # retcode 1 means: command failed to perform the requested operation
end

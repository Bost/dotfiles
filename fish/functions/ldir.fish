function ldir
  set cmd ls --color=auto -la -d1 */ $argv
  echo $cmd
  eval $cmd
end

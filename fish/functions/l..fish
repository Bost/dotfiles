function l.
  set cmd ls -d .* --color=auto $argv
  echo $cmd
  eval $cmd
end

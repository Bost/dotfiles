function hrep
  set cmd history --show-time="[%Y-%m-%d %H:%M:%S]\ " --search --contains (string escape -- $argv)
  echo $cmd
  eval $cmd
end

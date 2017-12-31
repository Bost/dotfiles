function gst
  set cmd "git status $argv"
  echo $cmd
  eval $cmd
end

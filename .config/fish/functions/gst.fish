function gst
  set cmd git status (string escape -- $argv)
  echo $cmd
  eval $cmd
end

function ys
  set cmd "youtube-dl --extract-audio $argv"
  echo $cmd
  eval $cmd
end

function ys
  # string delims needed - because of the url passed in the $argv
  set cmd "youtube-dl --extract-audio $argv"
  echo $cmd
  eval $cmd
end

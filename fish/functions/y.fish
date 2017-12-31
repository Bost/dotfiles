function y
  # string delims needed - because of the url passed in the $argv
  set cmd "youtube-dl --write-auto-sub --sub-lang 'fr' '$argv'"
  echo $cmd
  eval $cmd
end

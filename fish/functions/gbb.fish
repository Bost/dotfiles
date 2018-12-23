function gbb
  set cmd git bisect bad (string escape -- $argv)
  echo $cmd
  eval $cmd
end

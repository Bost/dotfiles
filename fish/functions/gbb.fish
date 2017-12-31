function gbb
  set cmd git bisect bad $argv
  echo $cmd
  eval $cmd
end

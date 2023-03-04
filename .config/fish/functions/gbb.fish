function gbb --description "git bisect bad"
  set cmd git bisect bad (string escape -- $argv)
  echo $cmd
  eval $cmd
end

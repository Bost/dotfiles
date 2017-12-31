function gss
  set cmd git status --short $argv
  echo $cmd
  eval $cmd
end

function gss
  set cmd git status --short (string escape -- $argv)
  echo $cmd
  eval $cmd
end

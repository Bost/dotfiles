# -*- mode: fish -*-

## fish -n m.fish
## fish_indent --check m.fish

function m --description "mplayer …"
  set cmd mplayer (string escape -- $argv)
  echo $cmd
  eval $cmd
end

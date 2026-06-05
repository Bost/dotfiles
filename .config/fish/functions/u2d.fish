# -*- mode: fish -*-

## fish -n u2d.fish
## fish_indent --check u2d.fish

function u2d --description "Convert line endings LF→CRLF (todos)"
    trace todos $argv
end

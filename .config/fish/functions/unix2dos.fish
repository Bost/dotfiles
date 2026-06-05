# -*- mode: fish -*-

## fish -n unix2dos.fish
## fish_indent --check unix2dos.fish

function unix2dos --description "Convert line endings LF→CRLF (todos)"
    trace todos $argv
end

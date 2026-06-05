# -*- mode: fish -*-

## fish -n d2u.fish
## fish_indent --check d2u.fish

function d2u --description "Convert line endings CRLF→LF (fromdos)"
    trace fromdos $argv
end

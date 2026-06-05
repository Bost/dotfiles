# -*- mode: fish -*-

## fish -n dos2unix.fish
## fish_indent --check dos2unix.fish

function dos2unix --description "Convert line endings CRLF→LF (fromdos)"
    trace fromdos $argv
end

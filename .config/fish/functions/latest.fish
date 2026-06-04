# -*- mode: fish -*-

## fish -n latest.fish
## fish_indent --check latest.fish

function latest
    set latestRepo (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
    cd ~/.cache/guix/checkouts/$latestRepo
end

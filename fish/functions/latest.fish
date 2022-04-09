function latest
    set latestRepo (ls --sort=time --almost-all ~/.cache/guix/checkouts/ | head -1)
    cd ~/.cache/guix/checkouts/$latestRepo
end

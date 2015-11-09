function crepg
    ack-grep \
    $dev/cheatsheet/git-commands.sh \
    --context=1 \
    --nogroup \
    --break --with-filename --color-filename=grey13 \
    --color-lineno=grey13 --color-match="bold blue" \
    --ignore-case --match $argv
end

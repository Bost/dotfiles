function crepr
    # --before-context=1 --after-context=1
    ack-grep \
    $dev/cheatsheet/rest-commands.sh \
    --context=1 \
    --nogroup \
    --break --with-filename --color-filename=grey13 \
    --color-lineno=grey13 --color-match="bold blue" \
    --ignore-case --match $argv
end

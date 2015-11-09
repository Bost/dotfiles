function crep -d "TODO all crep*.fish should use the same code basis"
    # --before-context=1 --after-context=1
    ack-grep \
    $dev/cheatsheet/git-commands.sh \
    $dev/cheatsheet/rest-commands.sh \
    $dev/cheatsheet/clojure-commands.clj \
    $dev/cheatsheet/emacs-commands.js \
    --context=1 \
    --nogroup \
    --break --with-filename --color-filename=grey13 \
    --color-lineno=grey13 --color-match="bold blue" \
    --ignore-case --match $argv
end

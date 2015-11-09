function crepc
    set -x _pwd (pwd)
    set -x _oldpwd $OLDPWD
    cd $dev/cheatsheet/
    ack-grep clojure-commands.clj \
             --context=1 \
             --nogroup \
             --break --with-filename --color-filename=grey13 \
             --color-lineno=grey13 --color-match="bold blue" \
             --ignore-case --match $argv
    cd $_pwd
    set -x OLDPWD $_oldpwd
end

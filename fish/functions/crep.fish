function crep -d "TODO crep/cr and creph should use the same code basis"
    set -x _pwd (pwd)
    set -x _oldpwd $OLDPWD
    cd $dev/cheatsheet/
    # --before-context=1 --after-context=1
    ack-grep git-commands.sh rest-commands.sh \
             clojure-commands.clj emacs-commands.js \
             --context=1 \
             --nogroup \
             --break --with-filename --color-filename=grey13 \
             --color-lineno=grey13 --color-match="bold blue" \
             --ignore-case --match $argv
    cd $_pwd
    set -x OLDPWD $_oldpwd
end

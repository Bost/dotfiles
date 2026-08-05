# -*- mode: fish -*-

## fish -n guix-repl-c.fish
## fish_indent --check guix-repl-c.fish

function guix-repl-c --description \
    'Eval a Scheme string via guix repl, banner-free (like guile -c <string>)'
    echo $argv[1] | guix repl /dev/stdin
end

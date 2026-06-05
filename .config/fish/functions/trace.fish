# -*- mode: fish -*-

## fish -n trace.fish
## fish_indent --check trace.fish

function trace --description "Trace command execution"
    printf '+%s %s\n' "$STARSHIP_PROMPT_SYMBOL" \
        (string join ' ' -- (string escape -- $argv))
    # NOTE: `command $argv` runs $argv as a flat argument list, with no
    # re-parsing. That keeps quoting correct (spaces / glob chars in args are
    # passed through verbatim, unlike `eval`), but it means trace can ONLY run
    # external programs. It cannot run:
    #   - fish builtins   (history, set, cd, …)
    #   - shell operators (pipes |, &&, ;, redirections)
    #   - constructs that rely on a second parse (eval-time glob/quote removal)
    # Wrappers around those must keep their own `eval`-based form.
    command $argv
end

# -*- mode: fish -*-

## fish -n config.fish
## fish_indent --check config.fish

#### config.fish manual configuration begin

# To reset the fish-shell back to it's initial state execute from bash:
# rm -rf ~/.config/fish/completions/
# rm -rf ~/.config/fish/conf.d/
# rm -rf ~/.config/fish/fish_plugins
# rm -rf ~/.config/fish/fish_variables
# rm -rf ~/.config/fish/functions/fisher.fish
# rm -rf ~/.config/fish/functions/tide.fish
# rm -rf ~/.config/fish/functions/tide/
# rm -rf ~/.config/fish/functions/fish_mode_prompt.fish
# rm -rf ~/.config/fish/functions/fish_prompt.fish
# rm -rf ~/.config/fish/functions/_tide_*

# Simple `set --erase fish_greeting` doesn't work, since fish_greeting is a
# function.
set fish_greeting ""

# --- GnuPG / direnv setup ---
# See `info "(gnupg) Invoking GPG-AGENT"`
set --global --export GPG_TTY (tty)

# Export empty DIRENV_LOG_FORMAT to silence direnv log output. See
# https://github.com/direnv/direnv/issues/68
#
# CAVEAT: this hides direnv's "is blocked"/"is denied" errors. See
# `direnv status` (look for "Found RC allowed 2" = denied) and fix with
# `direnv allow ~`
set --global --export DIRENV_LOG_FORMAT ""
direnv hook fish | source

# Set up fzf key bindings
fzf --fish | source

zoxide init fish | source

# ❯ψ Psi — resembles a fish skeleton / trident
# 🐟🐳🐠🎣🦑👽🛸🚀🧙🦊🐍💡🧠🤓👾🤖🦾🐌🐚

set --global --export STARSHIP_PROMPT_SYMBOL '🐠'
starship init fish | source

#### config.fish manual configuration end

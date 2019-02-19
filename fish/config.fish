set fish_greeting ""
set -U dev ~/dev
set -U dec ~/dec
set -U bin ~/bin

# set normal (set_color normal)
# set magenta (set_color magenta)
# set yellow (set_color yellow)
# set green (set_color green)
# set red (set_color red)
# set gray (set_color -o black)

# # Fish git prompt
# set __fish_git_prompt_showdirtystate 'yes'
# set __fish_git_prompt_showstashstate 'yes'
# set __fish_git_prompt_showuntrackedfiles 'yes'
# set __fish_git_prompt_showupstream 'yes'
# set __fish_git_prompt_color_branch yellow
# set __fish_git_prompt_color_upstream_ahead green
# set __fish_git_prompt_color_upstream_behind red

# # Status Chars
# set __fish_git_prompt_char_dirtystate '⚡'
# set __fish_git_prompt_char_stagedstate '→'
# set __fish_git_prompt_char_untrackedfiles '☡'
# set __fish_git_prompt_char_stashstate '↩'
# set __fish_git_prompt_char_upstream_ahead '+'
# set __fish_git_prompt_char_upstream_behind '-'

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# set -x PATH ~/.local/bin $PATH

# see /etc/profile.d/jdk.csh /etc/profile.d/jdk.sh /etc/environment ~/.config/fish/config.fish
# changes require logout and login
# set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set -x JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

set -x NODE_PATH ~/.config/yarn/global/node_modules

set -x ANDROID_HOME /usr/lib/android-sdk

# watch out! `/usr/local/bin/yarn --version` returns 0.27.5
set -x PATH $HOME/.yarn/bin $HOME/.config/yarn/global/node_modules/.bin $PATH

# vars for the cr*.fish functions:
# -P --perl-regexp
# -E --extended-regexp
# -z --null-data
# -o --only-matching
# -e PATTERN, --regepx=PATTERN
# -i --ignore-case
set -x sed0 's/^$/\n/'
set -x crep0 -Pzo
set -x lispline ";;.+\n(.*\n)*?\n"
set -x crep1 -Pzie
set -x shellline "#.+\n(.*\n)+?\n"

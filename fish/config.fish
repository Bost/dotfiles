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
# Warning! ~/.profile ignores existence of ~/bin
set -x PATH ~/bin ~/.local/bin $PATH

# JAVA_HOME definitions - see (changes require logout & login):
#     /etc/profile.d/jdk.csh
#     /etc/profile.d/jdk.sh
#     /etc/environment
#     ~/.config/fish/config.fish
# set -x JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set -x JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

# NODE_PATH definition might not be needed
set -x NODE_PATH ~/.config/yarn/global/node_modules
set -x ANDROID_HOME /usr/lib/android-sdk

# GraalVM comes with: openjdk version "1.8.0_202"
set -x GRAAL_HOME ~/graalvm-ce-1.0.0-rc13
# set -x GRAAL_HOME ~/graalvm-ce-1.0.0-rc12
if test -e $GRAAL_HOME
    # set -x PATH $GRAAL_HOME/bin $PATH
end

set cljjar ~/.m2/repository/org/clojure/clojure/1.10.0/clojure-1.10.0.jar
set cljsjar ~/.m2/repository/org/clojure/spec.alpha/0.2.176/spec.alpha-0.2.176.jar

set fish_greeting "set fish_greeting \"A note to myself\""
set --universal dev ~/dev
set --universal dec ~/dec
set --universal bin ~/bin

# Some spacemacs layers require certain tools to be available on $PATH
# see http://spacemacs.org/doc/FAQ.html#setup-path
# Warning! ~/.profile ignores existence of ~/bin
# Warning! the path to guix might be wrong. In bash it should be configured as:
#    export PATH="$HOME/.guix-profile/bin${PATH:+:}$PATH"
set --export PATH ~/.cabal/bin        $PATH
set --export PATH ~/.guix-profile/bin $PATH
set --export PATH ~/.yarn/bin         $PATH
set --export PATH ~/bin               $PATH
set --export PATH ~/.local/bin        $PATH

# JAVA_HOME definitions - see (changes require logout & login):
#     /etc/profile.d/jdk.csh
#     /etc/profile.d/jdk.sh
#     /etc/environment
#     ~/.config/fish/config.fish
# set --export JAVA_HOME /usr/lib/jvm/java-8-openjdk-amd64
# set --export JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

# NODE_PATH definition might not be needed
set --export NODE_PATH ~/.config/yarn/global/node_modules
set --export ANDROID_HOME /usr/lib/android-sdk

# GraalVM comes with: openjdk version "1.8.0_202"
set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc13
# set --export GRAAL_HOME ~/graalvm-ce-1.0.0-rc12
if test -e $GRAAL_HOME
    # set --export PATH $GRAAL_HOME/bin $PATH
end

# Remedy against:
# $ lein uberjar
# Release versions may not depend upon snapshots.
# Freeze snapshots to dated versions or set the LEIN_SNAPSHOTS_IN_RELEASE
# environment variable to override.
set --export LEIN_SNAPSHOTS_IN_RELEASE allowed

set cljjar ~/.m2/repository/org/clojure/clojure/1.10.0/clojure-1.10.0.jar
set cljsjar ~/.m2/repository/org/clojure/spec.alpha/0.2.176/spec.alpha-0.2.176.jar

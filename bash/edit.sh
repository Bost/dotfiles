#!/usr/bin/env bash

# debugging:
# set -x
# stop on error:
# set -e

if [ "$isLinuxVB" -gt 0 ]; then
    emacsCli=$HOME/emacs-24.4/lib-src/emacsclient
else
    emacsCli=emacsclient
fi
# echo "emacsCli: $emacsCli"
exec $emacsCli --alternate-editor="" -c "$@"

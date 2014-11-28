#!/usr/bin/env bash

# debugging:
# set -x
# stop on error:
# set -e

if [ $isLinuxNew64 -gt 0 ] || [ $isLinuxVB -gt 0 ]; then
    emacsCli=$dev/emacs/src/emacsclient
else
    emacsCli=emacsclient
fi
# echo "emacsCli: $emacsCli"
exec $emacsCli --alternate-editor="" -c "$@"

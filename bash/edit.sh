#!/usr/bin/env bash

# debugging:
# set -x
# stop on error:
# set -e

if [ $isLinuxFranzi -gt 0 ] || [ $isLinuxNew64 -gt 0 ] || [ $isLinuxVB -gt 0 ]; then
    emacsCli=$dev/emacs/lib-src/emacsclient
else
    emacsCli=emacsclient
fi
# echo "emacsCli: $emacsCli"
exec $emacsCli --alternate-editor="" -c "$@"

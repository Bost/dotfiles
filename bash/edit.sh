#!/usr/bin/env bash

# debugging:
# set -x
# stop on error:
# set -e

exec emacsclient --alternate-editor="" -c "$@"

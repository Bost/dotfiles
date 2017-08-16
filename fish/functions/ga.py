#!/usr/bin/env python

import common as c

dev = c.dev
cmds = [
    ["git", "--git-dir="+dev+"/cheatsheet/.git", "pull", "--rebase", "origin",],
    ["git", "--git-dir="+dev+"/dotfiles/.git",   "pull", "--rebase", "origin",],
    ["git", "--git-dir="+dev+"/git/.git",        "fetch", "--tags",],
    ["git", "--git-dir="+dev+"/fish-shell/.git", "fetch", "--tags",],
    ["git", "--git-dir="+dev+"/emacs-25/.git",   "fetch", "--tags",],
    ["git", "--git-dir=$HOME/.emacs.d/.git",     "pull",  "--rebase", "origin", "release-0.200",],
]

# w/o the map(...) into list(...) I get only # <map object at 0x7fa54f2b6048>
list(map(c.do_exec, cmds))

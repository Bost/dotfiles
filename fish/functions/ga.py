#!/usr/bin/env python

from subprocess import Popen, PIPE
import operator
import os
import re
import datetime
import time
from functools import reduce
from pathlib import Path

# return code index in the list returned by do_exec
idx_rc = 1
idx_res = 2

def safe_print(cmd_str):
    print(re.sub(r"--password\s+\S+", "--password ******", cmd_str))

def do_exec(cmd):
    cmd_str = " ".join(list(map(str, cmd)))
    safe_print(cmd_str)
    p = Popen(cmd_str, stdin=PIPE, stdout=PIPE, stderr=PIPE, shell=True)
    # output, err = p.communicate(b"input data that is passed to subprocess' stdin")
    output, err = p.communicate(b"")
    rc = p.returncode
    if rc != 0:
        print(err.decode("utf-8"))
    res = [cmd_str, rc, output]
    print(output.decode("utf-8"))
    return res

dev = str(Path.home())+"/dev"
cmds = [
    ["git", "--git-dir="+dev+"/cheatsheet/.git", "pull", "--rebase", "origin",],
    ["git", "--git-dir="+dev+"/dotfiles/.git",   "pull", "--rebase", "origin",],
    ["git", "--git-dir="+dev+"/git/.git",        "fetch", "--tags",],
    ["git", "--git-dir="+dev+"/fish-shell/.git", "fetch", "--tags",],
    ["git", "--git-dir="+dev+"/emacs-25/.git",   "fetch", "--tags",],
    ["git", "--git-dir=$HOME/.emacs.d/.git",     "pull",  "--rebase", "origin", "release-0.200",],
]

# w/o the map(...) into list(...) I get only # <map object at 0x7fa54f2b6048>
list(map(do_exec, cmds))

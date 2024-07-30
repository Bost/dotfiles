#!/usr/bin/env python

from subprocess import Popen, PIPE
import operator
import os
import re
import datetime
import time
from functools import reduce
from pathlib import Path

def map_f_reduce_add(f, xs):
    return reduce(operator.add, map(f, xs), []) # only map and reduce!

def tstp():
    """ See also:
    import logging
    logging.info("Foo")
    """
    return datetime.datetime.fromtimestamp(time.time()).strftime('%Y%m%d_%H%M%S')

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

# os.environ['...'] doesn't work
dev = str(Path.home())+"/dev"
dec = str(Path.home())+"/dec"

files = [
    "git.sh",
    "linux.sh",
    "win.bat",
    "clojure.clj",
    "emacs.el",
    "utf8.txt",
]

def links(f):
    target_dir = str(Path.home()) + "/dev/cheatsheet/cmds/"
    linkname_dir = target_dir+"crep/"
    cmd = ["ln", "--symbolic", target_dir+f, linkname_dir+f]
    do_exec(cmd)

# w/o the map(...) into list(...) I get only # <map object at 0x7fa54f2b6048>
list(map(links, files))

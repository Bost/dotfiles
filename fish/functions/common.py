#!/usr/bin/env python

from subprocess import Popen, PIPE
import os
import re
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

# os.environ['...'] doesn't work
dev = str(Path.home())+"/dev"
dec = str(Path.home())+"/dev/cljlein"

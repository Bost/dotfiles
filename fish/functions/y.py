#!/usr/bin/env python

import common as c
import sys

cmds = [
  ["youtube-dl", "--write-auto-sub", "--sub-lang", "'fr'",] + sys.argv[1:]
]

# w/o the map(...) into list(...) I get only # <map object at 0x7fa54f2b6048>
list(map(c.do_exec, cmds))

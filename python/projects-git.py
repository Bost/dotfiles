#!/usr/bin/env python

import common as c

dev = c.dev
dec = c.dec

prmsPO  = ["pull", "--rebase", "origin"]
prmsPOM = prmsPO + ["master"]
prmsFetch = ["fetch", "--tags",]

myRepos = [
     dec+"/zark"
    ,dec+"/ufo"
    ,dev+"/cheat"
    ,dev+"/dotfiles"
]

allRepos = myRepos + [
     dev+"/git"
    ,dev+"/fish-shell"
    ,dev+"/emacs-26"
    ,"$HOME/.emacs.d"
]

def cmdPOM(path):
    return(["git", "--git-dir=" + path + "/.git",] + prmsPOM)

# cmds = list(map(cmdPOM, myRepos))

# cmds = [
#     ["git", "--git-dir="+dec+"/zark/.git",      ] + prmsPOM,
#     ["git", "--git-dir="+dec+"/ufo/.git",       ] + prmsPOM,
#     ["git", "--git-dir="+dev+"/cheat/.git",] + prmsPOM,
#     ["git", "--git-dir="+dev+"/dotfiles/.git",  ] + prmsPOM,
#     ["git", "--git-dir="+dev+"/git/.git",       ] + prmsFetch,
#     ["git", "--git-dir="+dev+"/fish-shell/.git",] + prmsFetch,
#   # ["git", "--git-dir="+dev+"/emacs-26/.git",] + prmsFetch,
#     ["git", "--git-dir=$HOME/.emacs.d/.git",    ] + prmsPO + ["release-0.200",],
# ]

def cmdStatus(path):
    return(["git", "--git-dir=" + path + "/.git", "--work-tree="+path]
           + ["status", "--short", "--branch"])

# cmds = list(map(cmdStatus, myRepos[0:1]))
cmds = list(map(cmdStatus, myRepos))

# w/o the map(...) into list(...) I get only # <map object at 0x7fa54f2b6048>
list(map(c.do_exec, cmds))

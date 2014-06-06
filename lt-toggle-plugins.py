#!/usr/bin/env python

# TODO auto-detect links/plugins

from fabric.api import cd, env, prefix, run, task, local

pluginDir     = '~/.config/LightTable/plugins'
pluginDirDev  = '~/dev/lt-plugins-dev'
pluginDirOrig = '~/dev/lt-plugins-orig'

idxLinkName   = 0
idxTargetDev  = 1
idxTargetOrig = 2

plugins = [
     [pluginDir+'/Git_Status_Bar',    pluginDirDev+'/lt-gitstatusbar',    pluginDirOrig+'/Git_Status_Bar']
    ,[pluginDir+'/Syntax_Status_Bar', pluginDirDev+'/lt-syntaxstatusbar', pluginDirOrig+'/Syntax_Status_Bar']
    # ,['/home/bost/dev/my', 'my']
    ]

def do(cmd):
    # print(cmd)
    local(cmd)

def createLink(target, link):
    do('ln -s '+target+' '+link)

def removeLink(link):
    do('rm -f '+link)

def removeLinks():
    for p in plugins:
        linkName = p[idxLinkName]
        removeLink(linkName)

def create(idx):
    removeLinks()
    for p in plugins:
        target = p[idx]
        linkName = p[idxLinkName]
        createLink(target, linkName)

create(idxTargetOrig)
print pluginDir
local('ls -lAh --color '+pluginDir)


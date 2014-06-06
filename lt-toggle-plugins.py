#!/usr/bin/env python

# TODO auto-detect links/plugins

# from fabric.api import local
import os

pluginDir     = '~/.config/LightTable/plugins'
pluginDirDev  = '~/dev/lt-plugins-dev'
pluginDirOrig = '~/dev/lt-plugins-orig'

git='git'
syntax='syntax'
dev='dev'
orig='orig'

indexes = {dev : 1, orig : 2 } # indexes in the vector-values of pluginHashMap
idxLink = 0

plugins = {
    git :    [pluginDir+'/Git_Status_Bar',
              pluginDirDev+'/lt-gitstatusbar',
              pluginDirOrig+'/Git_Status_Bar'],
    syntax : [pluginDir+'/Syntax_Status_Bar',
              pluginDirDev+'/lt-syntaxstatusbar',
              pluginDirOrig+'/Syntax_Status_Bar']
    #, 'my'  : ['/home/bost/dev/my', 'my']
}

def do(cmd):
    # print(cmd)
    os.system(cmd)
    # local(cmd)

def createLink(target, link):
    do('ln -s '+target+' '+link)

def removeLink(link):
    do('rm -f '+link)

def activatePlugin(name, version):
    print 'name: '+name+'; version: '+version
    idxVersion = indexes[version]
    plugin   = plugins[name]
    target   = plugin[idxVersion]
    linkName = plugin[idxLink]
    removeLink(linkName)
    createLink(target, linkName)

# activatePlugin(git, dev)
# activatePlugin(syntax, dev)
os.system('ls -lAh --color '+pluginDir)

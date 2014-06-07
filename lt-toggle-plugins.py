#!/usr/bin/env python

# TODO auto-detect links/plugins

# from fabric.api import local
import os, sys

pluginDir     = '~/.config/LightTable/plugins'
pluginDirDev  = '~/dev/lt-plugins-dev'
pluginDirOrig = '~/dev/lt-plugins-orig'

git = 'git'
syntax = 'syntax'
my = 'my'

dev = 'dev'
orig = 'orig'
none = 'none' # None is a type of: types.NoneType

indexes = {dev : 1, orig : 2 } # indexes in the vector-values of pluginHashMap
idxLink = 0

plugins = {
    git    : [pluginDir+'/Git_Status_Bar',
              pluginDirDev+'/lt-gitstatusbar',
              pluginDirOrig+'/Git_Status_Bar'],
    syntax : [pluginDir+'/Syntax_Status_Bar',
              pluginDirDev+'/lt-syntaxstatusbar',
              pluginDirOrig+'/Syntax_Status_Bar'],
    my     : [pluginDir+'/my',
              pluginDirDev+'/my',
              None]
}

def do(cmd):
    # print(cmd)
    os.system(cmd)

def createLink(target, link):
    do('ln -s '+target+' '+link)

def removeLink(link):
    do('rm -f '+link)

def activatePlugin(pluginName, pluginVersion):
    # print 'name: '+pluginName+'; version: '+pluginVersion

    plugin    = plugins[pluginName]
    linkName  = plugin[idxLink]
    removeLink(linkName)

    if pluginVersion != none:
        idxVersion = indexes[pluginVersion]
        target = plugin[idxVersion]
        createLink(target, linkName)


plugin = sys.argv[1].lower()
version = sys.argv[2].lower()

# plugin = git
# version = dev

# plugin = syntax
# version = dev

# print 'plugin: '+plugin+'; version: '+version
activatePlugin(plugin, version)
os.system('ls -lAh --color '+pluginDir)

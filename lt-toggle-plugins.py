#!/usr/bin/env python

# TODO auto-detect links/plugins

from fabric.api import cd, env, prefix, run, task, local

baseDir = '~/dev/lighttable'
pluginDir = baseDir+'/deploy/plugins'

pairs=[
     ['/home/bost/dev/lt-gitstatusbar', 'lt-gitstatusbar']
    ,['/home/bost/dev/lt-syntaxstatusbar', 'lt-syntaxstatusbar']
    ,['/home/bost/dev/my', 'my']
    ]

def do(cmd):
    local(cmd)

def createLink(target, link):
    do('ln -s '+target+' '+link)

def removeLink(link):
    do('rm -f '+link)

def removeLinks():
    for tl in pairs:
        link = pluginDir+'/'+tl[1]
        removeLink(link)


def createLinks():
    for tl in pairs:
        target = tl[0]
        link = pluginDir+'/'+tl[1]
        createLink(target, link)


# removeLinks()
# createLinks()
do('ls -lAh --color '+pluginDir)

#!/usr/bin/env python
import os

def find(name, path):
    for root, dirs, files in os.walk(path):
        if name in files:
            return os.path.join(root, name)

def find_all(name, path):
    result = []
    for root, dirs, files in os.walk(path):
        if name in files:
            result.append(os.path.join(root, name))
    return result

import os, fnmatch

def find(pattern, path):
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name, pattern):
                result.append(os.path.join(root, name))
    return result

import re, sys, glob

def grep(search_str, files):
    print 'Current dir: '+os.getcwd()
    print 'Searching for: '+search_str
    print ''
    cntFilesFound = 0

    #for file in filter(os.path.isfile, sys.argv[2:]):
    for file in files:
        for lineNr, line in enumerate(open(file, 'r')):
            if re.search(search_str, line):
                # comma , after print removes carriage return (line will have one)
                print file+':'+str(lineNr)+': '+line,

    if cntFilesFound > 0:
        print 'Files found: '+str(cntFilesFound)

def findJavaCall():
    print 'Current dir: '+os.getcwd()
    print 'Searching for: '+search_str
    print ''
    dirs = [
        # TODO put hier dirnames where to search
        '',
        ]

    for d in dirs:
        filesFound = find('*.txt', d)
        grep('search_str', filesFound)

filesFound = find('*.txt', '/path/to/dir')
grep('search_str', filesFound)


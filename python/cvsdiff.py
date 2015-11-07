#!/usr/bin/env python

from subprocess import Popen, PIPE
import os
import re

files = [
# put a list of files here
]

# path to the "cvs.exe" file
#cvs = 'c:\Program Files (x86)\CVSNT\cvs.exe'
#cvs = 'c:\winapp-x86\cvsnt\cvs.exe'
cvs = 'cvs'

reRevision = r"revision (\S*)"
#reDate = r".*date: (...................);  author:"
reDate = r".*date: (....\/..\/..)"
reAuthor = r".*author: (.*);  state:"

firstOcc = 'first'
allOcc = 'all'

# keys
kRegExp = 're'
kOccurence = 'occurence'
kDestination = 'dest'

kFilename = 'filename'
kRevisions = 'revisions'
kAuthor = 'author'
kDate = 'date'

maxRevsToCheck = 4
# path to the directory where the old file are located
otherDir = ''

allRegExp = [
                { kRegExp : reRevision, kOccurence : allOcc   , kDestination : kRevisions },
                { kRegExp : reAuthor,   kOccurence : firstOcc , kDestination : kAuthor    },
                { kRegExp : reDate,     kOccurence : allOcc   , kDestination : kDate      },
                ]

def getDiff(fileInfo, regExpStructure, rev0, rev1=None):
    filename = fileInfo[kFilename]
    #print filename
    isFile = os.path.isfile(filename)
    if not isFile:
        return -1

    # compare local file with HEAD
    #command = [cvs, 'diff', filename]

    # compare revsion 1.14 and 1.19
    #          cvs   diff    -kk    -u    -r   1.14   -r   1.19  backend.c
    #command = [cvs, 'diff', '-kk', '-u', '-r', rev0, '-r', rev1, filename]

    # compare local file with 1.14
    #          cvs   diff    -kk    -u    -r   1.14   backend.c
    #command = [cvs, 'diff', '-kk', '-u', '-r', rev0, filename]
    command = [cvs, 'diff', '-r', rev0, filename]

    cmd = ''
    for c in command:
        cmd += c + ' '
    pipe = Popen(command, stdout=PIPE, stderr=PIPE, shell=False)

    # stderr for cvs diff does not work - i guess because of the different retval
    #errln = None
    #errLines = pipe.stderr.readlines()
    #print 'errlines: '+str(errLines)
    #for errln in errLines:
        #print filename
        #print errln

    #if errln:
        #return -1

    lines = pipe.stdout.readlines()
    #print cmd
    #print ''+filename+'          diffLen: '+str(len(lines))
    return len(lines)

def get(fileInfo, regExpStructure):
    filename = fileInfo[kFilename]
    command = [cvs, 'log', filename]
    #print filename
    pipe = Popen(command, stdout=PIPE, stderr=PIPE, shell=False)
    lines = pipe.stdout.readlines()

    for res in regExpStructure:
        occurence = res[kOccurence]
        destination = res[kDestination]
        if occurence == firstOcc and fileInfo[destination] != None:
            continue    # ignore rest of lines

        regExp = res[kRegExp]

        for line in lines:
            m = re.match(regExp, line)
            if m != None:
                mg = m.group(1)

                if occurence == firstOcc:
                    fileInfo[destination] = mg
                    break
                else:            #accumulate all occurencies and return them in an array
                    fileInfo[destination].append(mg)

    errln = None
    for errln in pipe.stderr.readlines():
        print filename
        print errln

    if errln:
        return None

    return fileInfo


diff = []   # not used at the moment
for filename in files:
    fileInfo = {
            kFilename : filename,
            kRevisions : [],
            kAuthor : None,
            kDate : [],
            }

    fileInfo[kFilename] = filename
    fileInfo = get(fileInfo, allRegExp)

    lastRev = '  last rev: '+fileInfo[kRevisions][0]+' '+fileInfo[kAuthor]+' '+fileInfo[kDate][0]
    year = fileInfo[kDate][0][0:4]
    if year < '2012':
        # a change not checked-in more than a year ago doesn't need to be back-ported
        continue

    #revs = ''
    #cnt = 0
    #for r in fileInfo[kRevisions]:
        #revs += r + ' '
        #if cnt == 0:
            #break
        #cnt += 1

    #print fileInfo[kFilename]+'    '+fileInfo[kAuthor]+'    '+fileInfo[kDate][0]+'   '+revs
    #diff.append(fileInfo)

    currDir = os.getcwd()
    os.chdir(otherDir)

    #print os.getcwd()
    revisions = fileInfo[kRevisions]
    cnt = 0
    match = 'none'
    for r in revisions:
        lenDiff = getDiff(fileInfo, allRegExp, r)
        if lenDiff == 0:
            match = r+' from '+fileInfo[kDate][cnt]
            break

        if cnt == maxRevsToCheck:
            break
        cnt += 1

    print fileInfo[kFilename]+lastRev+';                otherDir uses: '+match

    os.chdir(currDir)
    #print os.getcwd()

#!/usr/bin/env python

from subprocess import Popen, PIPE
import os
import re

files = [
# put a list of files here
]

# path to the "cvs.exe" file
#cvs = 'c:\Program Files (x86)\CVSNT\cvs.exe'
cvs = 'cvs'

reRevision = r"revision (\S*)"
reDate = r".*date: (...................);  author:"
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

allRegExp = [   
                { kRegExp : reRevision, kOccurence : allOcc   , kDestination : kRevisions },
                { kRegExp : reAuthor,   kOccurence : firstOcc , kDestination : kAuthor    },
                { kRegExp : reDate,     kOccurence : firstOcc , kDestination : kDate      },
                ]

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


diff = []
#for filename in filesTest:
for filename in files:
    fileInfo = {
            kFilename : filename,
            kRevisions : [],
            kAuthor : None,
            kDate : None,
            }

    fileInfo[kFilename] = filename
    fileInfo = get(fileInfo, allRegExp)

    revs = ''
    cnt = 0
    for r in fileInfo[kRevisions]:
        revs += r + ' '
        if cnt == 0:
            break
        cnt += 1

    print fileInfo[kFilename]+'    '+fileInfo[kAuthor]+'    '+fileInfo[kDate]+'   '+revs 
    diff.append(fileInfo)



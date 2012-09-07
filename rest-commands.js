var json = [
['rm -r ~/.cache/sessions',                       '# execute this the title bar dissapears from xfwm4'],
['ls --format=single-column',                     '&nbsp;'],
['echo "ls -l" | at midnight',                    '# Execute a command at a given time'],
['wc cheatsheet.html',                            '# line count, word count'],
['sudo !!',                                       '# Run the last command as root'],
['!$',                                            '# last parameter (argument) of the last command'],
['C-r, C-g',                                      '# bash history, abort history'],
['mv README.{text,txt} ; cp file{,.bak}',         '# mv README.text README.txt ; cp file file.bak'],
['ls -d1 */',                                     '# list only directories, 1 entry per line'],
['./command.sh 2&gt;&amp;1 | tee command.log',    '# print the output to log and to the stdout'],
['gvim $(find . -name "*fileToSearch*")',         '# find files and open them in gvim'],

['&nbsp;','&nbsp;'],

['find . -name "*fileToSearch*"',                 '&nbsp;'],
['find . -name *.properties -exec grep -lir ".*textToFind.*" \'{}\' \; -print',    '&nbsp;'],
['grep -lir "TextToFind" *',                      '# print only file-names'],
['grep sometext * | cut -f1 -d:',                 '# print only filenames of the files containing \'sometext\''],
['grep -i -n "TextToSearch" *',                   '# print line numbers'],
['grep --exclude=.git',                           '&nbsp;'],
['grep --exclude=.git -lir \'something\' *.cpp',  '&nbsp;'],

['&nbsp;','&nbsp;'],

['git filter-branch -f --env-filter "GIT_AUTHOR_NAME=\'Bost\'; GIT_AUTHOR_EMAIL=\'thebost@gmail.com\'; GIT_COMMITTER_NAME=\'Bost\'; GIT_COMMITTER_EMAIL=\'thebost@gmail.com\';" HEAD', '# change the name and email in all commits'],
['&nbsp;','&nbsp;'],
['git branch -rd public/whatever',                '# delete a remote-tracking branch from local repository'],
['git diff --name-only master branch',            '&nbsp;'],
['git show --name-only',                          '&nbsp;'],
['git log -S\'text-to-search\'',                  '# search entire commit history'],
['git log --pretty=format:\'%h %s\'',             '# show formated commit logs'],
['--git-dir=../all/.git --work-tree=.',           '# set git base directory and working tree'],


['&nbsp;','&nbsp;'],

['cvs diff -r RELEASE_1_0 -r RELEASE_1_1',        '&nbsp;'],
['cvs update -C path/file.ext',                   '# get clean copy'],
['cvs checkout -r branchOrTag module',            '# checkout module from branch or tag'],

['ssh-keygen',                                    '&nbsp;'],
['cat ~/.ssh/id_rsa.pub',                         '# now copy-paste the ~/.ssh/id_rsa.pub to github under \'Add another public key\''],

['METADA_CORE=.metadata/.plugins/org.eclipse.jdt.core;',                '&nbsp;'],
['rm -rf .metadata/.plugins/org.eclipse.core.resources/.history;',      '&nbsp;'],
['rm $METADA_CORE/*.index $METADA_CORE/savedIndexNames.txt;',           '&nbsp;'],

['&nbsp;','&nbsp;'],

['[\t ]+$',                                                             '# eclipse: remove trailing whitespaces'],


['&nbsp;','&nbsp;'],

['db2cmd -i -w db2clpsetcp',                                            '# db2: init envidonment'],
['echo %DB2CLP%',                                                       '&nbsp;'],
['db2 get connection state',                                            '&nbsp;'],
['db2 CATALOG TCPIP NODE $NODE_NAME REMOTE $IP_ADDR server $PORT',      '&nbsp;'],
['db2 CATALOG DATABASE $DATABASE_NAME AT NODE $NODE_NAME',              '&nbsp;'],
['db2 TERMINATE',                                                       '&nbsp;'],
['db2 -vf script.sql -t',                                               '# db2: launch script on CLI'],


['&nbsp;','&nbsp;'],


['\sr',                                                                 '# vimclojure: start REPL'],
['\sR',                                                                 '# vimclojure: start interactive REPL initialized to have same namespace as current buffer'],
['\si',                                                                 '# vimclojure: prompt for input and lookup with (source)'],
['\fd',                                                                 '# vimclojure: prompt for input and lookup with (find-doc)'],
['\el',                                                                 '# vimclojure: eval current line'],
['\ef',                                                                 '# vimclojure: eval current file'],
['\eb',                                                                 '# vimclojure: eval current current visual block selected'],
['\et',                                                                 '# vimclojure: send a function to the REPL'],
['\p',                                                                  '# vimclojure: close a window'],

['&nbsp;','&nbsp;'],

['/usr/ucb/ps -auxww',                                                  '# solaris: full command line'],
['jar tf',                                                              '# list files in a jar-file'],
['xfce4-session-logout',                                                '&nbsp;'],
['man -k abc',                                                          '# search man pages for abc'],
['$ssh-copy-id user@host',                                              '# Copy ssh keys to user@host to enable password-less ssh logins'],
['sshfs name@server:/path/to/folder /path/to/mount/point',              '# Mount folder/filesystem through SSH. Install SSHFS from http://fuse.sourceforge.net/sshfs.html. Will allow you to mount a folder security over a network.'],

['ssh user@host cat /path/to/remotefile | diff /path/to/localfile -',   '# Compare a remote file with a local file'],
]


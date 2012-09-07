var json = [
/*
Text Object (type :h text-objects in vim for a complete description)

This feature allows to operate on a block of text where the cursor is.

This is typically used with the 'd' or 'c' commands (delete, change), followed by 'i' or 'a' (inside a block or a whole block) and ended by a block description.

ca{ : Delete a block of code delimited by curly braces.   {   }
ci( : Change the content inside parenthesis.
ci" : Change the content inside a string
*/
[':bufdo %s/pattern/substit/ge | update',    '" substitute and <u>save</u> in all buffers'],
[':bnext (or bn)/ bprev (or bp)',            '" go to next/previous buffer'],
[':ball',                                    '" open all buffers'],
[':let @m=\'\'  /  :let @m=@n',              '" clear register / copy register value'],
['v/abc',                                    '" mark everything from the cursor up to abc'],
[':reg {arg}',                               '" display register number {arg}'],
['["x]y{motion}',                            '" yank {motion} into register x'],
['"qY',                                      '" store curret line to register q'],
['"0P',                                      '" paste last yanked text'],
[':messages',                                '" display error messages'],
['gq',                                       '" break lines according to :set textwidth. (see wrapmargin too)'],
['== / gg=G',                                '" format line / format file'],
['qq .... q @q/@@',                          '" start/stop recording, play down the recording'],
[':cw / :cwindow',                           '" open the quickfix window'],
[':cclose',                                  '" close quickfix window'],
[':wa',                                      '" write all buffers at once'],
[':marks, m',                                '" marks (for jumping etc); m - marks something?'],
['C-x C-k e',                                '" edit macro'],
['vim -p file1 file2',                       '" open vim with tabs'],
[':! sort',                                  '" &nbsp;'],
[':sort u',                                  '" filter out duplicate rows (unique rows only)'],
['78i-&lt;Esc&gt;',                          '" print 78 \'-\' chars at once'],
['gum gUm',                                  '" lowercase/uppercase of movement m'],
['R r ~',                                    '" replace mode, replace 1 char, replace char and move right'],
['30|',                                      '" jump to column 30'],
[':r! command',                              '" execute command and read in its output'],
['.!',                                       '" execute current line in the shell. This does not work somehow'],
['`. / \'.',                                 '" last edit location (~ key)/ line (&auml; key)'],
['; ,',                                      '" jump cursor forward, backward'],
['zz zb zt / M L H',                         '" move window / jump cursor middle, bottom, top'],
['zc zo za / zC zO zA / zR',                 '" close, open, toggle current / all folds from cursor/ all folds'],
[':set guifont=?',                           '" display current guifont'],
[':set guifont=*',                           '" display guifont dialog window'],
['C-^ / C-6',                                '" switch to the alternate file (probably the one marked with %/")'],
['C-t / C-d',                                '" align text in insert mode'],
['vit / yit / cit / dit',                    '" mark/yank/change/delete \"innerHTML\"'],
['vat / yat / cat / dat',                    '" mark/yank/change/delete whole tag'],

['ci{ / ci( / ci[ / ci"',                    '" change the content inside given parenthesis'],
['da&lt; / dap / daw',                       '" delete html tag / current paragraph (f.e. a function) / word'],
['o in visual mode',                         '" change the marking direction'],
['gv',                                       '" re-select last visual block'],
['I{string}&lt;Esc&gt; / A{string}&lt;Esc&gt;',                '" insert / append {string} to the visual block'],
['o (in visual mode)',                       '" change the marking direction in visual mode'],
['3/joe/e+1',                                '" find 3rd joe cursor set to End of match plus 1 [C]'],
['5/joe/s-2',                                '" find 5th joe cursor set to Start of match minus 2'],
['/joe/+4',                                  '" find joe cursor and move 4 lines down'],
['f/t F/T',                                  '" search forward / backward'],
['q: / q? / q/',                             '" open Ex command-line / search string'],
['c-f',                                      '" opens command-line history when editing search pattern'],
['* / "',                                    '" hightlight and search forwards/backwards'],
['g* / g"',                                  '" hightlight and search for parts of a word'],
[':[range]s/bacon/lettuce/[ciInp] [count]',  '" confirm, ignore case, case sensitive, number of matches; print lines'],
[':s/foo/bar/g10',                           '" replace only next 10 lines'],
[':[range]s//baz/...',                       '" use previous search pattern'],
[':%s/~/qux/igc',                            '" : Substitute your last replacement string'],
[':g/regexp/p',                              '" \"grep\"'],
['g; / g,',                                  '" cycle back / forward to the last places you were in insert mode'],
['s / S',                                    '" delete current char / clear current line and go to insert mode'],
['gf',                                       '" open file under cursor'],
['cc',                                       '" change current line'],
['C-o / C-i',                                '" retrace the movements in the file backwards / forwards'],
['C-a / C-x',                                '" increment / decrement next number on the current line'],
['xp',                                       '" swap chars'],
['C-n C-p',                                  '" word completition in insert mode'],
['C-x C-l',                                  '" line completition in insert mode'],
['/C-r C-w',                                 '" pull &lt;cword&gt; onto search/command line'],
['C-x C-o',		                             '" complete function name (omni completition)'],


['&nbsp;',                                   '" &nbsp;'],

[':diffthis',                                '" diff current two buffers'],
[':vert diffsplit filename',                 '" diff current buffer with a filename'],
['gvim -d file1 file2 / vimdiff file1 file2','" &nbsp;'],
[']c / [c',                                  '" jump to next / previous diff'],
['do :diffg :diffget',                       '" (diff obtain) replace a diff in the current viewport with that from the other viewport, use the :diffget command'],
['dp',                                       '" (diff put) replace a diff in the other viewport with that from the current viewport, use the :diffput command'],
['&nbsp;',                                   '&nbsp;'],

['set fileformat=dos|unix|mac',              '&nbsp;'],
['set filetype=html|xml|...',                '&nbsp;'],
['C-w 5&lt; / C-w 5&gt; / C-w 5- / C-w 5+',  '" resize vertical viewport 5 chars to the left / right / bottom / top'],
['C-w C-w',                                  '" move around split viewports'],
['C-w h/j/k/l',                              '" move around viewports according to given direction'],
['C-w r / R',                                '" rotate window down-&gt;right / up-&gt;left'],
['C-w o / :on',                              '" close other windows'],
['C-w |  /  C-w _',                          '" maximize vertically / horizontally'],
[':sp filename / :vsp filename',             '" split viewports horizontal / vertical'],
['&gt; / &lt; / =',                          '" shift text right / left / align text'],
['C-] / C-T',                                '" follow link / go back to previous topic in vim help'],
[':changes g; g,',                           '" move forth/back in the change list'],
[':jumps C-O / C-I',                         '" back/forth in the jump list (older/newer)'],

['&nbsp;',                                   '&nbsp;'],
['vim-fugitive',                             '&nbsp;'],
['-',                                        'stage / unstage current file'],
[':Gread',                                   'git checkout -- filename'],
[':Gmove',                                   'git mv'],
[':Gremove',                                 'git rm'],
['&nbsp;',                                   '&nbsp;'],
['NERDTree',                                 '&nbsp;'],
[':NERDTree q:',                             '" change drive to q: under windows'],
['i',                                        '" open split'],
['cd',                                       '" change dir'],
['r',                                        '" refresh'],
['B',                                        '" bookmarks'],
['&nbsp;',                                   '&nbsp;'],
['snipMate',                                 '" html tag completition'],
['fuzzyFinder',                              '" finds file, need L9 vim plugin (does not work somehow :('],
]


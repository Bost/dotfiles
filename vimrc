" TODO frames made of unicode chars
" TODO wikipedia table comparision
" TODO organize vimrc according to https://github.com/skwp/dotfiles/blob/master/vimrc

" {{{ Environment detection: see how is it made in bash
let isLinux = has('unix') && !has('win32unix')
let isCygwin = has('win32unix')
let isWin = has('win32')
let isEclim = 1

" {{{ File structure of .vim, .vimrc, vimfiles:
" isCygwin:
" ln -s $HOME/dev/dotfiles/vim .vim
" ln -s $HOME/dev/dotfiles/vimrc .vimrc
"
" isWin:
" cd $HOME
" create vimfiles.lnk pointing to $HOME\dev\dotfiles\vim
" }}}

"echo "isLinux: " isLinux
"echo "isCygwin: " isCygwin
"echo "isWin: " isWin
"echo "isEclim: " isEclim

"echo "has('mac'):" has('mac')
"echo "has('unix'):" has('unix')
"echo "has('win32'):" has('win32')
"echo "has('win32unix'):" has('win32unix')
" }}}

set nocompatible               " be iMproved
" 'set shortmess' doesn't work if called before 'set nocompatible'
set shortmess+=I               " Don't show the Vim welcome screen.
filetype off                   " required!

if has('win32')
    set langmenu=en_US.UTF-8
    let $LANG = 'en_US'

    "source $VIMRUNTIME/delmenu.vim
    "source $VIMRUNTIME/menu.vim

    set runtimepath+=$HOME/dev/dotfiles/vim
    set runtimepath+=$HOME/dev/dotfiles/vim/bundle/vundle
else
    set runtimepath+=~/.vim/bundle/vundle/
endif

call vundle#rc()

if has('win32')
    let g:bundle_dir = expand('$HOME/dev/dotfiles/vim/bundle', 1)
"else - on cygwin and linux everything works as expected
endif

" let Vundle manage Vundle - required!
Bundle 'gmarik/vundle'

" {{{ Plugings:
if isCygwin || isWin
    Bundle 'Bost/vim-email.git'
endif
Bundle 'Lokaltog/vim-easymotion'
Bundle 'rstacruz/sparkup', {'runtimepath': 'vim/'}
" vim-scripts repos

" YankRing colides with ctrlp
Bundle 'vim-scripts/YankRing.vim.git'

" {{{ Finder plugins:
" L9 is required by FuzzyFinder
"Bundle 'L9'
"Bundle 'FuzzyFinder'

" Let the ctrlp overwrite the <C-p> used by YankRing
Bundle 'kien/ctrlp.vim.git'

" LustyExplorer - requires Ruby
"Bundle 'sjbach/lusty.git'

" Command-T - requires Ruby
"Bundle 'git://git.wincent.com/command-t.git'
"Bundle 'wincent/Command-T.git'
" }}}

" python_ifold doesn't work somehow
"Bundle 'python_ifold'

Bundle 'skyl/vim-config-python-ide.git'

Bundle 'vim-scripts/csv.vim.git'

" {{{ Buffer Explorers:
Bundle 'c9s/bufexplorer.git'

" {{{ MiniBufExplorer is just bugging me
"Bundle 'fholgado/minibufexpl.vim.git'
"autocmd VimEnter * <Plug>CMiniBufExplorer
"autocmd MiniBufExplorer VimEnter       * call <SID>DEBUG('-=> VimEnter AutoCmd', 10) |let g:miniBufExplorerAutoUpdate = 1 |call <SID>AutoUpdate(-1,bufnr("%"))

" no auto load - it destroys the saved session; use \b to load it
"let g:miniBufExplorerNoAutoLoad = 1

" move in windows using C-w h/j/k/l
"let g:miniBufExplMapWindowNavVim = 1

" move in windows using C-w left / down / up / right
"let g:miniBufExplMapWindowNavArrows = 0

" C-Tab / C-S-Tab
"let g:miniBufExplMapCTabSwitchBufs = 1

" for other explorers like TagList
"let g:miniBufExplModSelTarget = 1

" Show a list of all open buffers with MiniBufExplorer
"map <Leader>b :TMiniBufExplorer<cr>

" }}}

" }}}

Bundle 'scrooloose/nerdcommenter.git'
Bundle 'scrooloose/nerdtree.git'
Bundle 'vim-scripts/VimClojure.git'
"Bundle 'hsitz/VimOrganizer.git'

Bundle 'sjl/gundo.vim.git'
Bundle 'xolox/vim-session.git'
"Bundle 'tpope/vim-rails.git'
Bundle 'tpope/vim-unimpaired.git'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround.git'
Bundle 'tpope/vim-repeat.git'
Bundle 'mileszs/ack.vim.git'

Bundle 'tsaleh/vim-matchit.git'
" powerline does no refresh when saving .vimrc; restart needed
Bundle 'Lokaltog/vim-powerline.git'

" Easily interact with tmux from vim
"Bundle 'benmills/vimux.git'

" majutsushi/tagbar seems to be nicer than vim-scripts/taglist;
" both plugins need exuberant ctags
Bundle 'majutsushi/tagbar'
"Bundle 'vim-scripts/taglist.vim.git'

Bundle 'vim-scripts/SearchComplete.git'
Bundle 'ervandew/supertab.git'
Bundle 'Bost/vim-zoom.git'

" Vim over ssh/scp
"Bundle 'vim-scripts/netrw.vim.git'

" Align.git is needed by SQLUtilities.git
"Bundle 'vim-scripts/Align.git'
"Bundle 'vim-scripts/SQLUtilities.git'
"
" {{{ TODO Evaluate plugins:
"Bundle 'vim-scripts/The-Mail-Suite-tms.git'
"Bundle 'vim-scripts/showmarks--Politz.git'
"Bundle 'juanpabloaj/ShowMarks.git'
" Shows java file class, package in a tree as in IDEs. Java source browser.
" Rating 258/92, Downloaded by 7798
Bundle 'vim-scripts/JavaBrowser.git'

" Compile and run Java program Rating 273/145, Downloaded by 8783
"Bundle 'vim-scripts/JavaRun.git'

" }}}

" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" See :h vundle for more details or wiki for FAQ
" Comments after Bundle command are not allowed
" }}}

" behave mswin
behave xterm
filetype plugin indent on     " required!

"let mapleader = ","
"let mapleader = " "
set showcmd                     " display incomplete commands
set hidden                      " handle multiple buffers more efficiently
set laststatus=2                " Always show status line

" Ups, on cygwin I use /bin/sh, not bash. Strange
"set shell=/bin/bash\ --login

set encoding=utf-8

" {{{ visualbell
" Use null visual bell (no beeps or flashes) this doesn't work
"set visualbell t_vb=

" set 'novisualbell' works as I want
set novisualbell
" }}}

set virtualedit=block   " Makes visual block mode awesome

set scrolloff=3         " Context lines at top and bottom of display.
set sidescrolloff=5     " Context columns at left and right.
set sidescroll=1        " Number of chars to scroll when scrolling sideways.

" {{{ gVim - GUI only stuff
if has('gui_running')
    " Switch off menu (t); show: bottom scrollbar (b), toolbar (T)
    "set guioptions=tb
    set guioptions=t
    " Show : menu (m), bottom scrollbar (b), gray nactive menu items (g)
    "set guioptions=gbmT

    " TODO toggle guioptions
    nnoremap <Leader>T :set guioptions=gbmT<CR>
    inoremap <Leader>T <Esc>:set guioptions=gbmT<CR>i
endif
" }}}

if isLinux
    " Ubuntu\ Mono\ 12 is too large for bambi-small
    set guifont=Ubuntu\ Mono\ 11
    "set guifont=Bitstream\ Vera\ Sans\ Mono\ 12
    "set guifont=DejaVu\ Sans\ Mono\ 12
elseif isCygwin
    set guifont=Bitstream\ Vera\ Sans\ Mono\ 8
elseif isWin
    "set guifont=Lucida_Console:h8:w5
    set guifont=Consolas:w5
endif
"set guifont=Monospace\ 9
"set guifont=Lucida_Console:h8:cDEFAULT
"set guifont=lucida_console:h8:w5
"set lines=64 columns=160

set cursorline          " highlight current line
" wrap long lines at 'breakat' not the last char on the screen; doesn't insert
" <EOLN>, it affects only the way how the files is displayed
"set linebreak

"colorscheme default
"colorscheme darkblue
try
    "colorscheme evening
    "colorscheme vividchalk
    "colorscheme vexorian
    "colorscheme autumn
    "colorscheme autumnleaf
    "colorscheme biogoo
    "colorscheme watermark
    "colorscheme wombat
    "colorscheme eclipse
    colorscheme eclipse-alternative
    "colorscheme wombatnew
    "colorscheme wombat256
    "colorscheme xoria254
    "colorscheme blackboard
    "colorscheme inkpot
catch
    "colorscheme slate
    colorscheme desert
endtry

set smartindent
"set autoindent

set tabstop=4
set expandtab                   " use spaces, not tabs
"set showtabline=2               " show the tabs right below the menu
set shiftwidth=4 softtabstop=4
set backspace=indent,eol,start  " backspace through everything in insert mode

" {{{ Switching from insert to normal mode <Leader><Leader>
"inoremap jj <Esc>
"inoremap :: <Esc>
" EasyMotion already uses <Leader><Leader>
"inoremap <Leader><Leader> <Esc>l
" }}}

" visualise a word and switch to insert mode
"map <space> viw
" visualise a word
"map <space> vw

" only for cheatsheet.html: jump to the 1st '#' character, then replace
" everything in the html tag with '# ' and then reinsert the '# ' string
"map <F3> f#cit#

" Use colours that work well on a dark background (Console is usually black)
"set background=dark

" Show EOL type and last modified timestamp, right after the filename
"set statusline=%<%F%h%m%r\ [%{&ff}]\ (%{strftime(\"%H:%M\ %d/%m/%Y\",getftime(expand(\"%:p\")))})%=%l,%c%V\ %P
" improve the help system, kind of tab completition
set wildmenu                    " Use menu for completions
set wildmode=full
" Make tab completion work more like it does in bash.
"set wildmode=longest,list

" {{{ VimClojure
"let vimclojure#FuzzyIndent=1
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
let vimclojure#DynamicHighlighting=1
let vimclojure#ParenRainbow=1
"let vimclojure#ParenRainbowColors = { '1': 'guifg=green' }
let vimclojure#WantNailgun = 1

if isLinux
    let vimclojure#NailgunClient = "ng"  "ng is defined in $PATH
elseif isWin || isCygwin
    let vimclojure#NailgunClient = $HOME.'/dev/vimclojure/client/ng.exe'
endif

"let vimclojure#NailgunServer = "192.168.178.20"  " 127.0.0.1
let vimclojure#NailgunPort = "2113"
let vimclojure#SplitPos = "right"        " open the split window on the right side
"let vimclojure#SplitSize = 80
" }}}

" {{{ Paste from system clipboard: <Leader>p
inoremap <A-p> <Esc>"*pa
"inoremap <S-Insert> <A-p>  " this does not work somehow
nnoremap <A-p> "*P
"inoremap <Leader>p <Esc>"*Pi
"nnoremap <Leader>p "*P
" }}}

" In insert mode: delete from cursor to the EOL and switch back to insert mode
inoremap <Leader>d <Esc>lDa

" Character coding for empty characters
set listchars=tab:▸\ ,eol:¶,extends:❯,precedes:❮,trail:_,nbsp:%

" Toggle hidden (empty) chars
nnoremap <Leader>sl :set list!<CR>
"set list

" Toggle line wrapping
nnoremap <Leader>wr :set wrap!<CR>

" {{{ Save file: <C-s>
nnoremap <C-s> :update<CR>
inoremap <C-s> <C-o>:update<CR>
vnoremap <C-s> <Esc>:update<CR>gv

"nnoremap <C-s> :w<CR>
"inoremap <C-s> <C-o>:w<CR>
" }}}

" Toggle line numbers
nnoremap <Leader>sn :set nu!<CR>

" Quit the current window
nnoremap <Leader>q :q<CR>

" Delete the current buffer
nnoremap <Leader>bd :bd<CR>
nnoremap <A-w> :bd<CR>
inoremap <Esc><A-w> :bd<CR>

" Show a list of all open buffers with BufExplorer
nnoremap <Leader>b :BufExplorer<CR>

" Quickly 'maximize' a split - these shortcuts colide with VimClojure
"nnoremap <Leader>mw <C-W>\|
"nnoremap <Leader>mh <C-W>_
"nnoremap <Leader>me <C-W>=
"nnoremap <Leader>mm <Leader>mw<Leader>mh

" Quickly 'maximize' the entire UI - 999 is a bit lot :(
nnoremap <Leader>Mw :set columns=250<CR>
nnoremap <Leader>Mh :set lines=69<CR>
nnoremap <Leader>MM <Leader>Mw<Leader>Mh

" {{{ Switch viewports: <C-hjkl>
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
" }}}


" {{{ Split up/down/left/right: not used
"nnoremap <Leader>sk :sp \| Explore<CR>
"nnoremap <Leader>sj :rightbelow sp \| Explore<CR>
"nnoremap <Leader>sh :vsp \| Explore<CR>
"nnoremap <Leader>sl :rightbelow vsp \| Explore<CR>
" }}}

" {{{ Editing dotfiles & cheatsheet files
"nnoremap <Leader>er :tabnew ~/.vimrc<CR>
nnoremap <Leader>ev :e ~/dev/dotfiles/vimrc<CR>
nnoremap <Leader>ec :e ~/dev/cheatsheet/vim-commands.js<CR>
nnoremap <Leader>er :e ~/dev/cheatsheet/rest-commands.js<CR>
nnoremap <Leader>ea :e ~/dev/dotfiles/bash/aliases<CR>
nnoremap <Leader>ee :e ~/dev/dotfiles/bash/env<CR>
" }}}

" {{{ .vimrc reloading: <Leader>r
" explicit reloading
nnoremap <Leader>rv :source ~/dev/dotfiles/vimrc<CR>

" Automatical reloading - slightly disturbing - use <Leader>S instead
"autocmd! bufwritepost ~/dev/dotfiles/vimrc source %
" }}}

" {{{ Quick evaluation: <Leader>S
" source current line
vnoremap <Leader>S y:execute @@<CR>:echo 'Sourced selection.'<CR>
" source highlighted text
nnoremap <Leader>S ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>
" }}}

" {{{ Sessions: not used at the moment
"nnoremap <Leader>rs :source ~/dev/mysite/mysession.vim<CR>
"nnoremap <Leader>so :OpenSession<CR>
"nnoremap <Leader>ss :SaveSession<CR>
"let g:session_autoload = 'no'
"let g:session_autosave = 'no'
" }}}

syntax on
filetype plugin indent on

" briefly jump to matching bracket
"set showmatch
" Substitute globaly
"set gdefault

"setlocal foldmethod=syntax
"setlocal foldmethod=marker " Steve Lohs uses this in his vimrc
"set foldlevel=1
set foldlevelstart=99          "remove folds
"set foldlevelstart=0

" {{{ Toggle folds: <Space>
nnoremap <Space> za
vnoremap <Space> za
" }}}

"set linespace=4                "add pixels between lines space for easy reading

augroup ft_java
    " Remove ALL autocommands for the current group.
    autocmd!

    autocmd FileType java setlocal foldmethod=marker
    autocmd FileType java setlocal foldmarker={,}
augroup END

augroup ft_vim
    " Remove ALL autocommands for the current group.
    autocmd!

    autocmd FileType vim setlocal foldmethod=marker
    autocmd FileType help setlocal textwidth=78
    autocmd BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END

set wrap
set relativenumber       " Shows relative line numbers
"set number               " Shows line number
set showmode             " show the current mode
set ruler                " Always show the cursor position.

" Searching
set hlsearch             " highlight matches
set incsearch            " incremental searching
set ignorecase           " searches are case insensitive...
set smartcase            " ... unless they contain at least one capital letter

" {{{ Center the display line after searches
nnoremap n nzz
nnoremap N Nzz

nnoremap * *zz
" dot not jump move on asterisk or hash
"nnoremap * *<c-o>
"nnoremap # #<c-o>

nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" Keep the working line in the center of the window. This is a toggle, so you
" can bounce between centered-working-line scrolling and normal scrolling by
" issuing the keystroke again.
"
" From this message on the MacVim mailing list:
" http://groups.google.com/group/vim_mac/browse_thread/thread/31876ef48063e487/133e06134425bda1?hl=enÂ¿e06134425bda1
"map <Leader>zz  :let &scrolloff=999-&scrolloff<CR>
" }}}

" {{{ Backups
set backupdir=/tmp
set directory=/tmp " Don't clutter up my dirs with swp and tmp files

"set nobackup
"set nowritebackup
"set noswapfile      " i.e. keep evething in memory
" }}}

" Wait-time for a key code or mapped keysequence to complete, default 1000
"set timeoutlen=500

" Horizontal split
nnoremap <Leader>- :sp<CR>
" Vertical split
nnoremap <Leader>+ :vsp<CR>

nnoremap <Leader>cdf :color default<CR>:colorscheme default<CR>
nnoremap <Leader>gs :Gstatus<CR>
" <CR> allows me to use this shortcut from the :Gstatus window
nnoremap <Leader>gf <CR>:Gdiff<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gh :Git push<CR>

" {{{ Convenience keybinding:

" {{{ Jump from window to window
nnoremap <Leader><Tab> <C-W>w
nnoremap <Tab> <C-W>w
" }}}

" {{{ Switch buffers: next: <C-Tab>, prev: <C-S-Tab>
noremap <C-Tab> :bnext<CR>
inoremap <C-Tab> <Esc>:bnext<CR>i

noremap <C-S-Tab> :bprevious<CR>
inoremap <C-S-Tab> <Esc>:bprevious<CR>i
" }}}

function! Vis(moveKey)
   let col = virtcol('.')
   let lineLen = virtcol('$')
   if col == lineLen-1 || col == 1
       normal V
   else
       if a:moveKey == 'j'
           normal vj
       elseif a:moveKey == 'k'
           normal vk
       else
           echo 'Unrecognized moveKey: ' a:moveKey
       endif
   endif
endfunction
nnoremap <Leader>c :call LookUpwards()<CR>

" {{{ Visualize: lines, words
nmap <S-Up> :call Vis('k')<CR>
imap <S-Up> <Esc>:call Vis('k')<CR>
vmap <S-Up> k

nmap <S-Down> :call Vis('j')<CR>
imap <S-Down> <Esc>:call Vis('j')<CR>
vmap <S-Down> j

nmap <S-Right> vl
imap <S-Right> <Esc>vl
nmap <C-S-Right> vw
imap <C-S-Right> <C-o>vw

nmap <S-Left> vh
imap <S-Left> <Esc>vh
nmap <C-S-Left> vb
imap <C-S-Left> <C-o>vb
" }}}

" {{{ Move lines up/down/left/right: <A-Up> / <A-Down> / <A-Left> / <A-Right>;
" requires unimpaired plugin
nmap <A-Up> [e
imap <A-Up> <C-o>[e
vmap <A-Up> [egv

nmap <A-Down> ]e
imap <A-Down> <C-o>]e
vmap <A-Down> ]egv

nmap <A-Left> <<
imap <A-Left> <Esc><<i
vmap <A-Left> <gv

nmap <A-Right> >>
imap <A-Right> <Esc>>>i
vmap <A-Right> >gv
" }}}

" {{{ DeleteWord-keybindings as in eclipse
imap <C-S-Del> <Esc>lDa
nmap <C-S-Del> D

nmap <C-BS> bde
imap <C-BS> <Esc>bdea

nmap <C-Del> de
imap <C-Del> <Esc>ldei
" }}}

" }}}

function! RenameFile()      " rename current file
    let old_name = expand("%")
    let new_name = input('New file name: ', expand('%'))
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
noremap <Leader>n :call RenameFile()<cr>

" {{{ Toggle line number counting: <Leader><C-L>
function! g:ToggleNuMode()
  if &nu == 1
     set rnu
  else
     set nu
  endif
endfunction
nnoremap <silent><Leader><C-L> :call g:ToggleNuMode()<CR>
" }}}

if isCygwin || isWin
    nnoremap <Leader>ma :call Email("Andreas")<CR>
    nnoremap <Leader>mj :call Email("Jürgen")<CR>
    nnoremap <Leader>my :call Email("Yvonne")<CR>
    nnoremap <Leader>mt :call Email("Thomas")<CR>

    nnoremap <Leader>ede :e $deployments_base/deployment.sh<CR>
    nnoremap <Leader>edv :e $deployments_base/defvars.sh<CR>
endif

" {{{ Jump To Buffer: <Leader>Number
nnoremap <Leader>0 :0b<CR>
nnoremap <Leader>1 :1b<CR>
nnoremap <Leader>2 :2b<CR>
nnoremap <Leader>3 :3b<CR>
nnoremap <Leader>4 :4b<CR>
nnoremap <Leader>5 :5b<CR>
nnoremap <Leader>6 :6b<CR>
nnoremap <Leader>7 :7b<CR>
nnoremap <Leader>8 :8b<CR>
nnoremap <Leader>9 :9b<CR>

nnoremap <Leader>10 :10b<CR>
nnoremap <Leader>11 :11b<CR>
nnoremap <Leader>12 :12b<CR>
nnoremap <Leader>13 :13b<CR>
nnoremap <Leader>14 :14b<CR>
nnoremap <Leader>15 :15b<CR>
nnoremap <Leader>16 :16b<CR>
" }}}

autocmd BufRead,BufNewFile *.cljs setlocal filetype=clojure

" {{{ Start maximized
if isLinux
    " this works when gvim -c "call Maximize_Window()"
    function! Maximize_Window()
        silent !wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz
    endfunction
elseif isCygwin
    " this doesn't really work
    "if has("gui_running")
        "" GUI is running or is about to start. Maximize gvim window.
        "set lines=999 columns=999
    "endif
elseif isWin
    autocmd GUIEnter * simalt ~x
endif
" }}}

" {{{ Better copy & paste: <F2>
" Insure Clean Pasting w/autoindented code - this is probably not usefull
"nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>

" Make the unnamed register to be the same as the "* register. Yand & Paste
" the selection without prepending with "* to commands
set clipboard=unnamed
" }}}

nnoremap <F3> :set hlsearch!<CR>
inoremap <F3> <Esc><F3>

" {{{ Ack and vimgrep: <Leader>a  <F4>
"nnoremap <Leader>a :Ack
" ! - means do not jump on the first occurence
nnoremap <Leader>a :Ack!<space>
let g:ackprg = 'ack --nogroup --nocolor --column'

" use <cword> to get the word under the cursor, and search for it in the
" current directory and all subdirectories; open the quickfix window when done
" In fact only 'j' (not jump to the firs location) should not print all
" matches
nnoremap <F4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cwindow<CR>
"noremap <F4> :execute "vimgrep /" . expand("<cword>") . "/gj **" <Bar> cwindow<CR>
inoremap <F4> <Esc><F4>
" }}}

nnoremap <F5> :GundoToggle<CR>
imap <F5> <Esc><F5>

nnoremap <F6> /
vnoremap <F6> /
inoremap <F6> <Esc><F6>

" execute current line and catch the output
noremap <F7> yyp!!sh<CR><Esc>
inoremap <F7> <Esc><F7>
noremap <Leader>l yypIls -la <Esc>yyp!!sh<CR><Esc>kk$
inoremap <Leader>l <Esc>yypIls -la <Esc>yyp!!sh<CR><Esc>kk$a

" {{{ Change slashes in the current line: <Leader>\ <Leader>/  (Win/Cygwin only)
if isWin || isCygwin
    nnoremap <silent> <Leader>/ :let tmp=@/<Bar>s:\\:/:ge<Bar>let @/=tmp<Bar>noh<CR>
    nnoremap <silent> <Leader><Bslash> :let tmp=@/<Bar>s:/:\\:ge<Bar>let @/=tmp<Bar>noh<CR>

    function! ToggleSlash(independent) range
        let from = ''
        for lnum in range(a:firstline, a:lastline)
            let line = getline(lnum)
            let first = matchstr(line, '[/\\]')
            if !empty(first)
                if a:independent || empty(from)
                    let from = first
                endif
                let opposite = (from == '/' ? '\' : '/')
                let nl = substitute(line, from, opposite, 'g')
                if from == '\'
                    let snl = substitute(nl, '\(.\):', '/cygdrive/\L\1', 'g')
                else
                    let snl = substitute(nl, '\\cygdrive\\\(.\)', '\u\1:', 'g')
                endif
                call setline(lnum, snl)
            endif
        endfor
    endfunction
    command! -bang -range ToggleSlash <line1>,<line2>call ToggleSlash(<bang>1)
    noremap <silent> <F8> :ToggleSlash<CR>
    inoremap <F8> <Esc><F8>
endif
" }}}

nnoremap <F9> :NERDTreeToggle<CR>
inoremap <F9> <Esc><F9>

" {{{ <F10>
autocmd FileType python map <F10> :w<CR>:!python "%"<CR>
inoremap <F10> <Esc><F10>
" }}}

" {{{ <F12>
nnoremap <F11> :YRShow<CR>
inoremap <F11> <Esc><F11>

"nnoremap <F12> :TlistToggle<CR>
nnoremap <F12> :TagbarToggle<CR>
"noremap <F12> :call VimCommanderToggle()<CR>
if isCygwin || isWin
    "nnoremap <F12> :silent !google-chrome ~/dev/cheatsheet/cheatsheet.html<CR>
else
    "nnoremap <F12> :silent !google-chrome ~/dev/cheatsheet/cheatsheet.html<CR>
endif
inoremap <F12> <Esc><F12>
" }}}

"autocmd VimEnter * NERDTree

" {{{ Show syntax highlighting groups for word under cursor: call SynStack()
nnoremap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
" }}}

" {{{ colorcolumn
" set colorcolumn=80
set colorcolumn=0
highlight ColorColumn guibg=black
" }}}

" {{{ Emacs-like beginning and end of line: <C-e> <C-a>
nnoremap <C-e> $
inoremap <C-e> <C-o>$

" nnoremap <C-a> ^  colides with that increment/decrement plugin
"nnoremap <C-a> ^
inoremap <C-a> <C-o>^
" }}}

" Select region from last edited line to the end of last pasted text
nnoremap <Leader>v '.V`]

" select charwise the contents of the curr line, excluding indentation.
" For pasting python line into REPL
nnoremap vv ^vg_

" {{{ Smart Home key: jump to the 1st nonblank char on the line, or, if
" already at that position, to the start of the line
noremap <expr> <silent> <Home> col('.') == match(getline('.'),'\S')+1 ? '0' : '^'
inoremap <silent> <Home> <C-O><Home>
" }}}

" Do not redraw while running macros (supposedly much faster)
set lazyredraw

" {{{ Kill trailing whitespace on save - this doesn't work somehow
"TODO this works: %s/\(\S\+\)\s\+\n/\1\r/gc
fun! <SID>StripTrailingWhitespaces()
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    call cursor(l, c)
endfun
autocmd FileType clj,javascript,java,python,readme,text,txt,vim
  \ autocmd BufWritePre <buffer>
  \ :call <SID>StripTrailingWhitespaces()
" }}}

" {{{ Scratch buffer (:enew replacement): <Leader>x
function! EditScratch()
    exec ':e /tmp/'.strftime("%Y-%m-%d_%H-%M-%S").'.scratch'
endfunction
noremap <Leader>x :call EditScratch()<CR>
" }}}

" {{{ Save some key strokes while doing substitution: <Leader>s
nnoremap <Leader>s :%s///g<left><left>
" }}}

" {{{ Show cursorline only in the current window and in normal mode
augroup cline
    autocmd!
    autocmd WinLeave,InsertEnter * set nocursorline
    autocmd WinEnter,InsertLeave * set cursorline
augroup END
" }}}

" {{{ TODO: Use sane regexes on command line: \v
"nnoremap / /\v
"vnoremap / /\v
" }}}

" gi already moves to "last place you exited insert mode", so we'll map gI to
" something similar: move to last change
nnoremap gI `.

" Autoread changed files
if isEclim
    set autoread
endif

nnoremap <Leader>wd :windo diffthis<CR>

" {{{ German Umlaute: <Leader>char (keyboard switching doesn't work in cygwin)
" Use the same keys as on a keyboard
inoremap <Leader>; ö
inoremap <Leader>: ö
inoremap <Leader>' ä
inoremap <Leader>" Ä
inoremap <Leader>[ ü
inoremap <Leader>{ Ü
inoremap <Leader>- ß
" }}}

" {{{ ctrlp settings
" Use the vim current directory
let g:ctrlp_working_path_mode = 0

"set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
"set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(class|jar|exe|so|dll|zip)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
" }}}

if isLinux
    let g:ackprg="/usr/bin/ack-grep -H --nocolor --nogroup --column"
endif

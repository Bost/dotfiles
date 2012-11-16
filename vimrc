" Initialization -------------------------------------------------------------
set shortmess+=I                " Don't show the Vim welcome screen.
set nocompatible               " be iMproved
filetype off                   " required!

"echo "has('mac'):" has('mac')
"echo "has('unix'):" has('unix')
"echo "has('win32'):" has('win32')
"echo "has('win32unix'):" has('win32unix')

if has('win32')
    set rtp+=$HOME/dev/dotfiles/vim/bundle/vundle
else
    set rtp+=~/.vim/bundle/vundle/
endif

call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" {{{ Plugings:
Bundle 'Bost/vim-email.git'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}
"Bundle 'tpope/vim-rails.git'
" vim-scripts repos

" {{{ finders:
" L9 is required by FuzzyFinder
"Bundle 'L9'
"Bundle 'FuzzyFinder'

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
" minibufexpl is just bugging me
"Bundle 'fholgado/minibufexpl.vim.git'
" }}}

Bundle 'scrooloose/nerdcommenter.git'
Bundle 'scrooloose/nerdtree.git'
Bundle 'tpope/vim-repeat.git'
Bundle 'xolox/vim-session.git'
Bundle 'tpope/vim-unimpaired.git'
Bundle 'vim-scripts/VimClojure.git'
"Bundle 'hsitz/VimOrganizer.git'

" YannkRing colides with ctrlp.vim.git
"Bundle 'vim-scripts/YankRing.vim.git'

Bundle 'sjl/gundo.vim.git'
Bundle 'tpope/vim-surround.git'
Bundle 'mileszs/ack.vim.git'

" powerline does no refresh when saving .vimrc; restart needed
Bundle 'Lokaltog/vim-powerline.git'

" Easily interact with tmux from vim
Bundle 'benmills/vimux.git'

" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
" }}}

" behave mswin
behave xterm
filetype plugin indent on     " required!

"let mapleader = ","
"let mapleader = " "
set showcmd                     " display incomplete commands
set hidden                      " handle multiple buffers more efficiently
set laststatus=2                " Always show status line
set encoding=utf-8
set visualbell t_vb=    " Use null visual bell (no beeps or flashes).

" Keep the working line in the center of the window. This is a toggle, so you
" can bounce between centered-working-line scrolling and normal scrolling by
" issuing the keystroke again.
"
" From this message on the MacVim mailing list:
" http://groups.google.com/group/vim_mac/browse_thread/thread/31876ef48063e487/133e06134425bda1?hl=enÂ¿e06134425bda1
"
map <Leader>zz  :let &scrolloff=999-&scrolloff<CR>

set virtualedit=block   " Makes visual block mode awesome

set scrolloff=3         " Context lines at top and bottom of display.
set sidescrolloff=5     " Context columns at left and right.
set sidescroll=1        " Number of chars to scroll when scrolling sideways.

" GUI only stuff -------------------------------------------------------------
if has('gui_running')
    " switch off the menu in the gui window
    set guioptions=gt

    " horizontal scrollbar
    set guioptions+=b

    " remove menubar
    "set go+=m
endif

if has('unix')
    set guifont=Bitstream\ Vera\ Sans\ Mono\ 8
    "set guifont=DejaVu\ Sans\ Mono\ 9
elseif has('win32unix')
    set guifont=Bitstream\ Vera\ Sans\ Mono\ 8
elseif has('win32')
    set guifont=Lucida_Console:h8:w5
elseif has('mac')
endif
"set guifont=Monospace\ 9
"set guifont=Lucida_Console:h8:cDEFAULT
"set guifont=lucida_console:h8:w5
"set lines=64 columns=160

set cursorline          " highlight current line
"set linebreak            " TODO what is linebreak for?

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
    colorscheme wombat
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

"imap jj <Esc>
"imap :: <Esc>

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


" Here's the vimclojure stuff. You'll need to adjust the NailgunClient
" setting if you're on windows or have other problems.
"let vimclojure#FuzzyIndent=1
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
let vimclojure#DynamicHighlighting=1
let vimclojure#ParenRainbow=1
"let vimclojure#ParenRainbowColors = { '1': 'guifg=green' }
let vimclojure#WantNailgun = 1
if has('win32') || has('win32unix')
    let vimclojure#NailgunClient = $HOME.'/dev/vimclojure/client/ng.exe'
else
    let vimclojure#NailgunClient = "ng"  "ng is defined in $PATH
endif
"let vimclojure#NailgunServer = "192.168.178.20"  " 127.0.0.1
let vimclojure#NailgunPort = "2113"
let vimclojure#SplitPos = "right"        " open the split window on the right side
"let vimclojure#SplitSize = 80

" Shortcuts ------------------------------------------------------------------

" Press twice the <Leader> to exit insert mode without a manual <ESC> or <C-C>
imap <Leader><Leader> <Esc>l

" paste from system clipboard in normal and insert mode
imap <Leader>v <Esc>"*P
nmap <Leader>v "*P

" in insert mode: delete from cursor to the EOL and switch back to insert mode
imap <Leader>d <Esc>lDa

" Character coding for empty characters
set listchars=tab:>-,eol:$,precedes:>,trail:_
" Set symbols for tabs like textmate

" Toggle hidden (empty) chars
nmap <Leader>l :set list!<CR>
"set list


" Toggle line wrapping
nmap <Leader>wr :set wrap!<CR>

" {{{ Save file
nmap <Leader>w :update<CR>
nmap <C-s> :update<CR>
imap <C-s> <Esc>:update<CR>i
imap <Leader>w <Esc>:update<CR>i

"nmap <Leader>w :w<CR>
"nmap <C-s> :w<CR>
"imap <C-s> <Esc>:w<CR>i
"imap <Leader>w <Esc>:w<CR>i
" }}}

" Sort selected text
vnoremap <Leader>s :sort<CR>

" Toggle line numbers
"nmap <Leader>n :set nu!<CR>

" Quit the current window
nmap <Leader>q :q<CR>

" Delete the current buffer
nmap <Leader>bd :bd<CR>

" Show a list of all open buffers with BufExplorer
nmap <Leader>b :BufExplorerHorizontalSplit<CR>

" Show a list of all open buffers with MiniBufExplorer
"nmap <Leader>b :ls<CR>
"map <Leader>b :TMiniBufExplorer<cr>

" Jump from window to window
nmap <Leader><Tab> <C-W>w

" Jump from tab to tab
nmap <Leader><S-Tab> :tabn<CR>

" Quickly 'maximize' a split - these shortcuts colide with VimClojure
"nmap <Leader>mw <C-W>\|
"nmap <Leader>mh <C-W>_
"nmap <Leader>me <C-W>=
"nmap <Leader>mm <Leader>mw<Leader>mh

" Quickly 'maximize' the entire UI - 999 is a bit lot :(
nmap <Leader>Mw :set columns=250<CR>
nmap <Leader>Mh :set lines=69<CR>
nmap <Leader>MM <Leader>Mw<Leader>Mh

" Switch windows with CTRL + hjkl
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l


" Split up/down/left/right
nmap <Leader>sk :sp \| Explore<CR>
nmap <Leader>sj :rightbelow sp \| Explore<CR>
nmap <Leader>sh :vsp \| Explore<CR>
nmap <Leader>sl :rightbelow vsp \| Explore<CR>

"nmap <Leader>er :tabnew ~/.vimrc<CR>
nmap <Leader>ev :e ~/dev/dotfiles/vimrc<CR>
nmap <Leader>ec :e ~/dev/cheatsheet/vim-commands.js<CR>
nmap <Leader>ea :e ~/dev/dotfiles/bash/aliases<CR>
nmap <Leader>ee :e ~/dev/dotfiles/bash/env<CR>

" {{{ .vimrc reloading
" explicit reloading
" nmap <Leader>r :source ~/dev/dotfiles/vimrc<CR>

" Automatical reloading
autocmd! bufwritepost ~/dev/dotfiles/vimrc source %

"nmap <Leader>rs :source ~/dev/mysite/mysession.vim<CR>
nmap <Leader>so :OpenSession<CR>
nmap <Leader>ss :SaveSession<CR>
" }}}

" this is the default mouse setting
"if has('mouse')
"    set mouse=a
"endif


" Syntax ---------------------------------------------------------------------
syntax on
filetype plugin indent on

" Sriefly jump to matching bracket
"set showmatch
" Substitute globaly
"set gdefault

"setlocal foldmethod=syntax
"set foldlevel=1
" set foldlevelstart=99          "remove folds

" TODO Test: Space to toggle folds
nnoremap <Space> za
vnoremap <Space> za

"set linespace=4                "add pixels between lines space for easy reading


" Editor Behavior -------------------------------------------------------------
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
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
" }}}

" {{{ backups
set backupdir=/tmp
set directory=/tmp " Don't clutter my dirs up with swp and tmp files

"set nobackup
"set nowritebackup
"set noswapfile      " i.e. keep evething in memory
" }}}

" Wait-time for a key code or mapped keysequence to complete, default 1000
"set timeoutlen=500

" Horizontal split
nmap <leader>- :sp<CR>
" Vertical split
"nmap <leader>\ :vsp<CR>

nmap <leader>cdf :color default<CR>:colorscheme default<CR>
nmap <leader>st :Gstatus<CR>
" <CR> allows me to use this shortcut from the :Gstatus window
nmap <leader>df <CR>:Gdiff<CR>
nmap <leader>gci :Gcommit<CR>
nmap <leader>gps :Git push<CR>

try                     " these keys are not mapped under windows by default
    unmap <S-Up>
    unmap <S-Down>
catch
endtry

nmap <S-Up> [e
nmap <S-Down> ]e

vmap <S-Left> <gv
vmap <S-Right> >gv
vmap <S-Up> [egv
vmap <S-Down> ]egv

" Shortcut for :bnext and :bprevious.
nmap <A-Left> :bprevious<CR>
nmap <A-Right> :bnext<CR>

" Remap 0 to first non-blank character
"map 0 ^

" Rename current file
function! RenameFile()
    let old_name = expand("%")
    let new_name = input('New file name: ', expand('%'))
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>

" use <leader>Ctrl+L to toggle the line number counting method
function! g:ToggleNuMode()
  if &nu == 1
     set rnu
  else
     set nu
  endif
endfunction
nnoremap <silent><leader><C-L> :call g:ToggleNuMode()<CR>

" Quickly switch buffers (prev/next)
" it doesn't work somehow TODO try to see why using :map or :nmap
"try
    "unmap <C-P>
    "unmap <C-N>
"catch
"endtry
"nmap <C-P> :bp<CR>
"nmap <C-N> :bp<CR>


" win32unix is for cygwin
if has('win32') || has('win32unix')
    nmap <leader>ma :call Email("Andreas")<CR>
    nmap <leader>mj :call Email("Jürgen")<CR>
    nmap <leader>my :call Email("Yvonne")<CR>
    nmap <leader>mt :call Email("Thomas")<CR>

    nmap <leader>ed :e $deployments_base/deployment.sh<CR>
endif

" {{{ Jump To Buffer
nmap <leader>0 :0b<CR>
nmap <leader>1 :1b<CR>
nmap <leader>2 :2b<CR>
nmap <leader>3 :3b<CR>
nmap <leader>4 :4b<CR>
nmap <leader>5 :5b<CR>
nmap <leader>6 :6b<CR>
nmap <leader>7 :7b<CR>
nmap <leader>8 :8b<CR>
nmap <leader>9 :9b<CR>

nmap <leader>10 :10b<CR>
nmap <leader>11 :11b<CR>
nmap <leader>12 :12b<CR>
nmap <leader>13 :13b<CR>
nmap <leader>14 :14b<CR>
nmap <leader>15 :15b<CR>
nmap <leader>16 :16b<CR>
nmap <leader>17 :17b<CR>
nmap <leader>18 :18b<CR>
nmap <leader>19 :19b<CR>
nmap <leader>20 :20b<CR>
nmap <leader>21 :21b<CR>
nmap <leader>22 :22b<CR>
nmap <leader>23 :23b<CR>
nmap <leader>24 :24b<CR>
nmap <leader>25 :25b<CR>
nmap <leader>26 :26b<CR>
nmap <leader>27 :27b<CR>
nmap <leader>28 :28b<CR>
nmap <leader>29 :29b<CR>
nmap <leader>30 :30b<CR>
nmap <leader>31 :31b<CR>
nmap <leader>32 :32b<CR>
nmap <leader>33 :33b<CR>
nmap <leader>34 :34b<CR>
nmap <leader>35 :35b<CR>
nmap <leader>36 :36b<CR>
nmap <leader>37 :37b<CR>
nmap <leader>38 :38b<CR>
nmap <leader>39 :39b<CR>
" }}}

autocmd BufRead,BufNewFile *.cljs setlocal filetype=clojure

" start maximized
if has('win32')
    au GUIEnter * simalt ~x
else
    " this works when gvim -c "call Maximize_Window()"
    function! Maximize_Window()
        silent !wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz
    endfunction
endif

" {{{ better copy & paste
" Insure Clean Pasting w/autoindented code - this is probably not usefull
"nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set clipboard=unnamed
" }}}

"nmap <F2> :set hlsearch!<CR>

" use <cword> to get the word under the cursor, and search for it in the
" current directory and all subdirectories; open the quickfix window when done
" In fact only 'j' (not jump to the firs location) should not print all
" matches
map <F4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cwindow<CR>
"map <F4> :execute "vimgrep /" . expand("<cword>") . "/gj **" <Bar> cwindow<CR>

" execute current line and catch the output
map <F8> yyp!!sh<CR><Esc>

nnoremap <F5> :GundoToggle<CR>
nnoremap <silent> <F9> :NERDTreeToggle<CR>
nnoremap <silent> <F11> :YRShow<CR>
"noremap <silent> <F12> :call VimCommanderToggle()<CR>
nmap <F12> :silent !google-chrome ~/dev/cheatsheet/cheatsheet.html<CR>
" Start python on F5
autocmd FileType python map <F5> :w<CR>:!python "%"<CR>

"autocmd VimEnter * <Plug>CMiniBufExplorer
"autocmd VimEnter * NERDTree

"autocmd MiniBufExplorer VimEnter       * call <SID>DEBUG('-=> VimEnter AutoCmd', 10) |let g:miniBufExplorerAutoUpdate = 1 |call <SID>AutoUpdate(-1,bufnr("%"))

let g:session_autoload = 'no'
let g:session_autosave = 'no'

" no auto load - it destroys the saved session; use \b to load it
let g:miniBufExplorerNoAutoLoad = 1

" move in windows using C-w h/j/k/l
let g:miniBufExplMapWindowNavVim = 1

" move in windows using C-w left / down / up / right
"let g:miniBufExplMapWindowNavArrows = 0

" C-Tab / C-S-Tab
let g:miniBufExplMapCTabSwitchBufs = 1

" for other explorers like TagList
"let g:miniBufExplModSelTarget = 1


" Show syntax highlighting groups for word under cursor
nmap <C-S-P> :call <SID>SynStack()<CR>
function! <SID>SynStack()
    if !exists("*synstack")
        return
    endif
    echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" {{{ colorcolumn
" set colorcolumn=80
set colorcolumn=0
highlight ColorColumn guibg=black
" }}}

" {{{ Emacs-like beginning and end of line
nmap <c-e> $
imap <c-e> <c-o>$

" colides with that plugin for increment/decrement
"nmap <c-a> ^
imap <c-a> <c-o>^
" }}}

" Select region from last edited line to the end of last pasted text
nnoremap <leader>v '.V`]

" Open the grep replacement
nnoremap <leader>a :Ack

" Do not redraw while running macros (much faster)
set lazyredraw

" {{{ Kill trailing whitespace on save - this doesn't work somehow
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
      call setline(lnum, substitute(line, from, opposite, 'g'))
    endif
  endfor
endfunction
command! -bang -range ToggleSlash <line1>,<line2>call ToggleSlash(<bang>1)
noremap <silent> <F8> :ToggleSlash<CR>

" {{{
function! EditScratch()
    "let fName = ':e /tmp/'.strftime("%Y-%m-%d_%H-%M-%S").'.scratch'
    exec ':e /tmp/'.strftime("%Y-%m-%d_%H-%M-%S").'.scratch'
endfunction

map <Leader>x :call EditScratch()<CR>
" }}}

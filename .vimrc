" behave mswin
behave xterm

set number
set hlsearch
set incsearch
set ignorecase
set smartcase

"set guifont=DejaVu\ Sans\ Mono\ 9
"set guifont=Lucida_Console:h8:cDEFAULT
"set guifont=Monospace
"set guifont=lucida_console:h8:w5
"set lines=64 columns=160

" highlight current line
set cursorline

" TODO what is linebreak for?
"set linebreak

colorscheme default

" horizontal scrollbar
set guioptions+=b 

filetype indent on
filetype plugin on

set smartindent
"set autoindent

" character coding for empty characters
"set listchars=tab:>-,eol:$,precedes:>,trail:_

" show empty characters
"set list

set tabstop=4
set shiftwidth=4 softtabstop=4

" start maximized
" autocmd GUIEnter * :simalt ~x

"imap jj <Esc>
imap :: <Esc>

set showtabline=2

set backspace=indent,eol,start  " make that backspace key work the way it should
"set background=dark             " Use colours that work well on a dark background (Console is usually black)
set showmode                    " show the current mode
syntax on                       " turn syntax highlighting on by default

" Show EOL type and last modified timestamp, right after the filename
set statusline=%<%F%h%m%r\ [%{&ff}]\ (%{strftime(\"%H:%M\ %d/%m/%Y\",getftime(expand(\"%:p\")))})%=%l,%c%V\ %P

" This is standard pathogen and vim setup
set nocompatible
call pathogen#infect()
syntax on

" Here's the vimclojure stuff. You'll need to adjust the NailgunClient
" setting if you're on windows or have other problems.
"let vimclojure#FuzzyIndent=1
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
let vimclojure#DynamicHighlighting=1
let vimclojure#ParenRainbow=1
"let vimclojure#ParenRainbowColors = { '1': 'guifg=green' }
let vimclojure#WantNailgun = 1
let vimclojure#NailgunClient = $HOME . "/.vim/lib/vimclojure-nailgun-client/ng"


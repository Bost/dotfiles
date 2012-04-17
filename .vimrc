" behave mswin
behave xterm

set number
set hlsearch
set incsearch
set ignorecase
set smartcase

"set guifont=DejaVu\ Sans\ Mono\ 9
"set lines=64 columns=160
set cursorline

"colorscheme evening
set guioptions+=b 

filetype indent on
filetype plugin on

set smartindent
"set autoindent

" show empty characters
"set list

set tabstop=4
set shiftwidth=4 softtabstop=4

" start maximized
" autocmd GUIEnter * :simalt ~x

"imap jj <Esc>
imap :: <Esc>


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


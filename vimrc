" Basics
set nocompatible
syntax on
set encoding=utf-8

" Vim UI
set incsearch
set showmatch
set showcmd
set ruler

" Text Formatting/Layout
set ignorecase
set smartcase

" Tabs, Indentation
set textwidth=79
set tabstop=2
set shiftwidth=2
set cindent
set autoindent
set smarttab
set expandtab
set backspace=indent,eol,start

" Filetypes
filetype off
filetype indent plugin off
set runtimepath+=$GOROOT/misc/vim
filetype on
filetype indent plugin on
autocmd FileType ruby set shiftwidth=2
autocmd FileType c set shiftwidth=4 tabstop=4
autocmd FileType yacc set shiftwidth=4 tabstop=4
autocmd FileType go set shiftwidth=4 tabstop=4

"
" Re-mappings
"

let mapleader = ","

" Control + S in insert mode: switch to normal mode and write to disk.
inoremap <C-S> <ESC>:w<CR>

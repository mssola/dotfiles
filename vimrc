" Basics
set nocompatible
syntax on

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
filetype on
filetype indent on
filetype plugin on

autocmd FileType ruby set shiftwidth=2
autocmd FileType c set shiftwidth=4 tabstop=4


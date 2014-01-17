""
" Basics

filetype on
set nocompatible
syntax on
set encoding=utf-8

""
" Global stuff

" Show matched result.
set showmatch

" Don't show the current command in the lower right corner.
set showcmd

" Show numbers.
set number

" Don't show the mode, since this will be shown by the status line.
set noshowmode

" Make sure that unsaved buffers that are to be put in the background are
" allowed to go in there (ie. the "must save first" error doesn't come up)
set hidden

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Don't wrap lines
set nowrap

" I don't like the backup and the swapfile thingies.
set nobackup
set noswapfile

""
" Colorscheme

set background=dark
colorscheme xoria256

""
" Setup the cursor for Konsole.
" NOTE: change this if you're not using Konsole or a program that makes use
" of the Konsole part. See
" http://vim.wikia.com/wiki/Change_cursor_shape_in_different_modes

let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

""
" Status line

" Returns the mode we are currently.
function! Mode()
  redraw
  let l:mode = mode()

  if     mode ==# "n" | return "NORMAL"
  elseif mode ==# "i" | return "INSERT"
  elseif mode ==# "R" | return "REPLACE"
  elseif mode ==# "v" | return "VISUAL"
  elseif mode ==# "V" | return "VISUAL-LINE"
  elseif mode ==# "s" | return "SELECT"
  elseif mode ==# ""  | return "VISUAL-BLOCK"
  else                | return l:mode
  endif
endfunc

" My own status line.
set stl=\ %{Mode()}\ \|\ %f\ %m\ %r\ %=\ Line:\ %l\ Column:\ %v\ Buf:#%n

" Always show the status line.
set laststatus=2

""
" Text Formatting/Layout

set ignorecase
set smartcase

""
" Tabs, Indentation

set textwidth=79
set tabstop=2
set shiftwidth=2
set cindent
set autoindent
set smarttab
set expandtab
set backspace=indent,eol,start

""
" Plugins.

filetype off
filetype indent plugin off

" Using the Go plugins from the official repo.
if isdirectory($GOROOT . "/misc/vim")
  set runtimepath+=$GOROOT/misc/vim
endif

" All the plugins are listed in my bundles file, that will be installed
" inside the ~/.vim directory.
source ~/.vim/bundles.vim

""
" Filetypes

autocmd FileType ruby set shiftwidth=2
autocmd FileType c set shiftwidth=4 tabstop=4
autocmd FileType yacc set shiftwidth=4 tabstop=4
autocmd FileType go set shiftwidth=4 tabstop=4

""
" Re-mappings

let mapleader = ","

" Make window navigation easier.
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <leader>c :close<CR>

" <C-S> in normal/insert mode: switch to normal mode and write to disk.
inoremap <C-S> <ESC>:w<CR>
nnoremap <C-S> :w<CR>

" <C-A> in normal mode selects the whole text by using visual mode.
nnoremap <C-A> ggVG

" vim-surround.vim: <leader>s is used to surround a word.
map <leader>s ysiw


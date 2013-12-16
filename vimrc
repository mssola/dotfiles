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
" must save first error doesn't come up)
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

""
" Re-mappings

let mapleader = ","

" Control + S in insert mode: switch to normal mode and write to disk.
inoremap <C-S> <ESC>:w<CR>
nnoremap <C-S> :w<CR>
nnoremap <C-A> ggVG

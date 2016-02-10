""
" Basics

filetype on
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
set relativenumber

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

" Use always the clipboard.
" NOTE: this only works if you have vim with clipboard support. You can check
" this by calling vim --version and see if you have +clipboard.
set clipboard=unnamed

" Paste toggle
set pastetoggle=<f2>

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
" Searching stuff.

" Ignore the case on search by default.
set ignorecase

" Case sensitive if we start the search by an uppercase letter.
set smartcase

" Find as you type search.
set incsearch

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

" All the plugins are listed in my bundles file, that will be installed
" inside the ~/.vim directory.
source ~/.vim/bundles.vim

" Enable plugins again.
filetype indent plugin on

""
" Everything regarding filetypes: indentation rules, gofmt & friends.

source ~/.vim/autocmd.vim

""
" Re-mappings

let g:mapleader = ","

" Make window navigation easier.
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
nnoremap <leader>c :close<CR>
nnoremap <leader>v :vs<CR>
nnoremap <leader>V :vs<CR><C-w><C-l>

" <C-s> in normal/insert mode: switch to normal mode and write to disk.
inoremap <C-s> <ESC>:w<CR>
nnoremap <C-s> :w<CR>

" <C-q> is the same as @q (I use q to store macros).
nnoremap <C-q> @q

" Pressing ^ in order to move to the first non-blank character is inconvenient.
nnoremap <leader>, ^

" Do nothing when <C-z> is pressed. Why would I want to stop the current `vim`
" process ? (especially shitty if you miss useful commands like <C-x> ...).
nnoremap <C-z> <nop>
inoremap <C-z> <nop>

" Do nothing for the <S-K> command. I never use it and it appears when I
" misstype <S-J>.
nnoremap K <nop>

" Do nothing for <c-o>. I don't use this feature and it confuses me whenever I
" misstype <c-p> (CtrlP plugin).
nnoremap <c-o> <nop>

" Set paste temporarily to just paste what we have on the registry.
nnoremap <leader>p :set paste<cr>p:set nopaste<cr>


" Vundle setup
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" My list of plugins.
Bundle 'tpope/vim-surround.git'
Bundle 'kien/ctrlp.vim.git'
Bundle 'derekwyatt/vim-scala.git'

""
" Some small adjustments for the installed plugins.

" vim-surround.vim: <leader>s is used to surround a word.
map <leader>s ysiw

" Let CtrlP's window to be taller than its default configuration.
let g:ctrlp_match_window = 'max:40'


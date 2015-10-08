" Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

" My list of plugins.
Plugin 'tpope/vim-surround.git'
Plugin 'kien/ctrlp.vim.git'
Plugin 'derekwyatt/vim-scala.git'
Plugin 'fatih/vim-go'

call vundle#end()

""
" Some small adjustments for the installed plugins.

" vim-surround.vim: <leader>s is used to surround a word.
map <leader>s ysiw

" Let CtrlP's window to be taller than its default configuration.
let g:ctrlp_match_window = 'max:40'

" Tell vim-go to use goimports on save.
let g:go_fmt_command = "goimports"

" Execute go-lint on save.
if isdirectory($GOPATH."/src/github.com/golang/lint/misc/vim")
  set rtp+=$GOPATH/src/github.com/golang/lint/misc/vim
  autocmd BufWritePost,FileWritePost *.go execute 'Lint' | cwindow
endif

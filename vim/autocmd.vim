" Indentation rules for different languages.

" Some rules regarding indentation on different languages.
autocmd FileType sh setlocal shiftwidth=4 tabstop=4
autocmd FileType perl setlocal shiftwidth=4 tabstop=4
autocmd FileType c setlocal shiftwidth=4 tabstop=4
autocmd FileType yacc setlocal shiftwidth=4 tabstop=4
autocmd FileType go setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType ruby setlocal shiftwidth=2 tabstop=2
autocmd FileType cpp setlocal shiftwidth=4 tabstop=4
autocmd FileType php setlocal noexpandtab shiftwidth=4 tabstop=4
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2

" Clean up trailing whitespaces on save.
autocmd BufWritePre * :%s/\s\+$//e

" If there is a .lvimrc, source it. Note that this works best by calling vim on
" the root of your project.
if filereadable(".lvimrc")
  so .lvimrc
endif

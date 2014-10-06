
" Indentation rules for different languages.

" Some rules regarding indentation on different languages.
autocmd FileType sh set shiftwidth=4 tabstop=4
autocmd FileType perl set shiftwidth=4 tabstop=4
autocmd FileType c set shiftwidth=4 tabstop=4
autocmd FileType yacc set shiftwidth=4 tabstop=4
autocmd FileType go set noexpandtab shiftwidth=4 tabstop=4
autocmd FileType ruby set shiftwidth=2 tabstop=2
autocmd FileType cpp set shiftwidth=4 tabstop=4
autocmd FileType php set noexpandtab shiftwidth=4 tabstop=4
autocmd FileType javascript set shiftwidth=2 tabstop=2

" Call gofmt on save.
autocmd FileType go autocmd BufWritePre <buffer> Fmt

" Clean up trailing whitespaces on save.
autocmd BufWritePre * :%s/\s\+$//e



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

" Call gofmt on save.
autocmd FileType go autocmd BufWritePre <buffer> Fmt

" Clean up trailing whitespaces on save.
autocmd BufWritePre * :%s/\s\+$//e


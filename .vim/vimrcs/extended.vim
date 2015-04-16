""""""""""""""""""""""""""
" Persistent undo
" so you can undo even after closing a buffer/VIM
""""""""""""""""""""""""""
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
    set undofile
catch
endtry

""""""""""""""""""""""""""
" => Parenthesis/bracket
""""""""""""""""""""""""""
" Map auto complete of (, ", ', [
inoremap $1 ()<esc>i
inoremap $2 []<esc>i
inoremap $3 {}<esc>i
inoremap $4 {<esc>o}<esc>O
inoremap $q ''<esc>i
inoremap $e ""<esc>i
inoremap $t <><esc>i

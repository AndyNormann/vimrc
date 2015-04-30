" Compiling and running commands for certain languages
"
"""""""""""""""""""""""
" => Python
"""""""""""""""""""""""
au FileType python nnoremap <leader>r :!clear;python3 %<cr>
au FileType python nnoremap <leader>t :!clear;python3 % 

"""""""""""""""""""""""""""""
" => C section
""""""""""""""""""""""""""""""
au FileType c nnoremap <Leader>t :!clear && ./%< 
au FileType c nnoremap <Leader>c :!clear && gcc -Wall -Wextra -o %< %<CR>
au FileType c inoremap <Leader>. ->

""""""""""""""""""""""""""""
" => Cpp section
"""""""""""""""""""""""""""""
au FileType cpp nnoremap <Leader>t :!clear && ./%< 
au FileType cpp nnoremap <Leader>c :!clear && g++ -std=c++11 -Wall -Wextra -o %< %<CR>
au FileType cpp inoremap <Leader>. ->

""""""""""""""""""""""""""""""
" => Java section
"""""""""""""""""""""""""""""""
au FileType java nnoremap <Leader>t :!clear && java %< 
au FileType java nnoremap <Leader>c :!clear && javac *.java<CR>

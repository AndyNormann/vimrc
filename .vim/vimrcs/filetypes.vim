" Compiling and running commands for certain languages
"
"""""""""""""""""""""""
" => Python
"""""""""""""""""""""""
au FileType python nnoremap <leader>r :!python3 %<cr>
au FileType python nnoremap <leader>t :!python3 % 

"""""""""""""""""""""""
" => Ruby
"""""""""""""""""""""""
au FileType ruby nnoremap <leader>r :!ruby %<cr>
au FileType ruby nnoremap <leader>t :!ruby % 

"""""""""""""""""""""""""""""
" => C section
""""""""""""""""""""""""""""""
au FileType c nnoremap <Leader>t :!./%< 
au FileType c nnoremap <Leader>c :!gcc -Wall -Wextra -o %< %<CR>
au FileType c inoremap <Leader>. ->

""""""""""""""""""""""""""""
" => Cpp section
"""""""""""""""""""""""""""""
au FileType cpp nnoremap <Leader>t :!./%< 
au FileType cpp nnoremap <Leader>c :!g++ -std=c++14 -Wall -Wextra -o %< %<CR>
au FileType cpp inoremap <Leader>. ->
au FileType cpp inoremap ., ::

""""""""""""""""""""""""""""""
" => Java section
"""""""""""""""""""""""""""""""
au FileType java nnoremap <Leader>t :!java %< 
au FileType java nnoremap <Leader>c :!javac *.java<CR>

""""""""""""""""""""""""""""""
" => Go section
"""""""""""""""""""""""""""""""
au FileType go nnoremap <Leader>c :!go build %<cr>
au FileType go nnoremap <Leader>t :!./%< 
au FileType go nnoremap <Leader>r :!go run %<cr>

"""""""""""""""""""""""""""""
" => Rust section
""""""""""""""""""""""""""""""
au FileType rust nnoremap <Leader>c :!rustc %<cr>
au FileType rust nnoremap <Leader>t :!./%< 
au FileType rust nnoremap <Leader>r :!./%<<cr>
au FileType rust inoremap ., ::
au FileType rust nnoremap <Leader>b :!cargo build<cr>
au FileType rust nnoremap <Leader>v :!cargo run<cr>

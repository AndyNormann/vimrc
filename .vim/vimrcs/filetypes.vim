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

"""""""""""""""""""""""
" => Elm
"""""""""""""""""""""""
au FileType elm nnoremap <leader>c :ElmMake<cr>

"""""""""""""""""""""""""""""
" => C section
""""""""""""""""""""""""""""""
au FileType c nnoremap <Leader>t :!./%< 
"au FileType c nnoremap <Leader>c :!clang -Weverything -o %< %<CR>
au FileType c nnoremap <Leader>c :AsyncRun clang -Weverything -o %:t:r %:t
au FileType c inoremap <Leader>. ->

""""""""""""""""""""""""""""
" => Cpp section
"""""""""""""""""""""""""""""
au FileType cpp nnoremap <Leader>t :!/tmp/%<
"au FileType cpp nnoremap <Leader>t :!./%< 
au FileType cpp nnoremap <Leader>c :AsyncRun clang++ -Weverything -std=c++14 -o /tmp/%:t:r %
"au FileType cpp nnoremap <Leader>c :!g++ -std=c++14 -Wall -Wextra -o %< %<CR>
au FileType cpp inoremap <Leader>. ->
au FileType cpp inoremap ., ::

""""""""""""""""""""""""""""""
" => Java section
"""""""""""""""""""""""""""""""
au FileType java nnoremap <Leader>t :!java %< 
au FileType java nnoremap <Leader>c :AsyncRun javac *.java

""""""""""""""""""""""""""""""
" => Go section
"""""""""""""""""""""""""""""""
au FileType go nnoremap <Leader>c :AsyncRun go run %
au FileType go nnoremap <Leader>p :GoToggleBreakpoint<cr>
au FileType go nnoremap <Leader>r :GoDebug<cr>

"""""""""""""""""""""""""""""
" => Rust section
""""""""""""""""""""""""""""""
au FileType rust inoremap ., ::
"au FileType rust nnoremap <Leader>b :!cargo build<cr>
"au FileType rust nnoremap <Leader>v :!cargo run<cr>
au FileType rust nnoremap <Leader>c :AsyncRun cargo build<cr>
au FileType rust nnoremap <Leader>t :AsyncRun cargo run<cr>

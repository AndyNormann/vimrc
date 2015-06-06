set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'

Bundle 'honza/vim-snippets'
Bundle 'garbas/vim-snipmate'
Bundle 'rking/ag.vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'szw/vim-g'
Bundle 'justinmk/vim-sneak'
Bundle 'scrooloose/nerdcommenter'
Bundle 'itchyny/lightline.vim'
Bundle 'Shougo/unite.vim'

call vundle#end()

" Google things
nmap <C-g> :Google <c-r>=expand("%:e")<cr> 

" Lightline config
            "\ 'colorscheme': 'Tomorrow_Night_Bright',
            "\ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
let g:lightline = {
            \ 'colorscheme': 'jellybeans',
            \ 'active': { 
            \   'left': [ ['mode'], ['filename', 'modified', 'readonly', 'line']],
            \   'right': []
            \ },
            \ 'inactive': {
            \   'left': [['filename', 'modified', 'readonly']],
            \   'right': []
            \ },
            \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
            \ 'component_function': {
            \   'mode': 'LLMode',
            \   'readonly': 'MyReadonly',
            \ }
            \}


" Function for setting the text for modes in lightline
function! LLMode()
  return
        \ lightline#mode() == 'NORMAL' ? 'N' :
        \ lightline#mode() == 'INSERT' ? 'I' :
        \ lightline#mode() == 'VISUAL' ? 'V' :
        \ lightline#mode() == 'V-LINE' ? 'V' :
        \ lightline#mode() == 'V-BLOCK' ? 'V' :
        \ lightline#mode() == 'REPLACE' ? 'R' : lightline#mode()
endfunction

" Sets a fancy symbol if the file is readonly
function! MyReadonly()
    return &readonly ? '' : ''
endfunction

" Unite config
call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_grep_command = 'ag' "'ag --nocolor --nogroup -g'
let g:unite_source_line_enable_highlight = 1
let g:unite_force_overwrite_statusline = 0

"" Unite binds
map <C-b> :Unite buffer<cr>
map <C-f> :Unite line -start-insert<cr>
map <C-e> :Unite file_rec<cr>

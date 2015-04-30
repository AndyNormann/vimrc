set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'

Bundle 'bling/vim-airline'
Bundle 'honza/vim-snippets'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'garbas/vim-snipmate'
Bundle 'Shougo/unite.vim'
Bundle 'scrooloose/syntastic'
Bundle 'bling/vim-bufferline'

call vundle#end()


" GOod theme let g:airline_theme = 'murmur'
"let g:airline_theme = 'kolor'
"let g:airline_theme = 'molokai'
"let g:airline_theme = 'monochrome'
"let g:airline_theme = 'raven'
" Airline config
let g:airline_powerline_fonts = 1
let g:airline_theme = 'understated'
let g:airline_extensions = ['bufferline', 'syntastic']
let g:airline#extensions#default#layout = [
            \ [ 'a', 'c' ],
            \ [ 'warning' ]
            \ ]

" Bufferline config
let g:bufferline_echo = 0
let g:bufferline_show_bufnr = 0

" Unite config
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_grep_command = 'ag' "'ag --nocolor --nogroup -g'
let g:unite_source_line_enable_highlight = 1
" makes unite behave with airline
let g:unite_force_overwrite_statusline = 0

" Makes syntastic not check for errors when you're quiting vim
let g:syntastic_check_on_wq = 0
let g:syntastic_check_on_open = 0
let g:syntastic_enable_signs = 0

" Rebinds / to unite search in file and ? to search in working dir + sub-directories
nnoremap ? :Unite -start-insert -auto-resize file file_rec<CR>
nnoremap / :Unite -start-insert -auto-resize line<cr>
" <c-n> let's you resume where you left off in your previous search
nnoremap <silent><c-n> :<C-u>UniteResume<CR>

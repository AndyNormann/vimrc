set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'

Bundle 'flazz/vim-colorschemes'
Bundle 'bling/vim-airline'
Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'junkblocker/unite-codesearch'
Bundle 'Shougo/neomru.vim'
Bundle 'ujihisa/unite-colorscheme'
Bundle 'Shougo/unite-outline'
Bundle 'scrooloose/syntastic'



call vundle#end()


" Airline config
let g:airline_powerline_fonts = 1
let g:airline_theme = 'lucius'

" Set key for ultisnips
let g:UltiSnipsExpandTrigger="<tab>"

" Unite config
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_cursor_line_highlight = 'CursorLine'
let g:unite_source_codesearch_command = '/usr/local/bin/csearch'
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_file_mru_filename_format = ':~:.'
let g:unite_source_file_mru_time_format = ''
let g:unite_source_grep_command = 'ag'

" Syntastic config
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

nnoremap ? :Unite -start-insert -auto-resize file file_rec/async file_mru<CR>
nnoremap / :Unite -start-insert -auto-resize line<cr>
nnoremap <silent><c-n> :<C-u>UniteResume<CR>
nnoremap <c-p> :Unite -start-insert -auto-resize codesearch<cr>
nnoremap <leader>p :Unite outline<cr>

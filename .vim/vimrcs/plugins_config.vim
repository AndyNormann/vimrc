set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'

Bundle 'honza/vim-snippets'
"Bundle 'garbas/vim-snipmate'
Bundle 'SirVer/ultisnips'
Bundle 'rking/ag.vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'tomtom/tlib_vim'
Bundle 'szw/vim-g'
Bundle 'itchyny/lightline.vim'
Bundle 'Shougo/unite.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'fatih/vim-go'
Bundle 'flazz/vim-colorschemes'
Bundle 'tpope/vim-fugitive'
Bundle 'Valloric/YouCompleteMe'

call vundle#end()

" Google things
nmap <C-g> :Google <c-r>=expand("%:e")<cr> 

" Lightline config
let g:lightline = {
            \ 'colorscheme': 'solarized',
            \ 'active': { 
            \   'left': [['mode'], ['filename']],
            \   'right': []
            \ },
            \ 'inactive': {
            \   'left': [['filename']],
            \   'right': []
            \ },
            \ 'component_function': {
            \   'readonly': 'MyReadonly',
            \   'filename': 'LightLineFilename'
            \ }
            \}


" Help functions for filename function
function! LightLineModified()
  if &filetype == "help"
    return ""
  elseif &modified
    return "+"
  elseif &modifiable
    return ""
  else
    return ""
  endif
endfunction

function! LightLineReadonly()
  if &filetype == "help"
    return ""
  elseif &readonly
    return "⭤"
  else
    return ""
  endif
endfunction
" Gives all filename information in the filename
function! LightLineFilename()
  return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
       \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
       \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

" Unite config
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_grep_command = 'ag' "'ag --nocolor --nogroup -g'
let g:unite_source_line_enable_highlight = 1
let g:unite_force_overwrite_statusline = 0

"" Unite binds
map <C-b> :Unite buffer<cr>
map <C-f> :Unite line -start-insert<cr>
map <C-e> :Unite file_rec<cr>

"" NERDTree settings
let NERDTreeRespectWildIgnore=1
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=0
nmap \ :NERDTreeToggle<cr>

" YouCompleteMe
let g:ycm_filetype_blacklist = {}
let g:ycm_show_diagnostics_ui = 0
let g:ycm_cache_omnifunc = 0
" Makes youcompleteme not use the preview window
set completeopt=menuone
let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
"" Ultisnips
let g:UltiSnipsJumpForwardTrigger="<tab>"

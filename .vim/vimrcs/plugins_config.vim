set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()

"Bundle 'gmarik/Vundle.vim'
set runtimepath+=~/.vim/bundle/neobundle.vim/
call neobundle#begin(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'honza/vim-snippets'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'rking/ag.vim'
NeoBundle 'MarcWeber/vim-addon-mw-utils'
NeoBundle 'tomtom/tlib_vim'
NeoBundle 'szw/vim-g'
NeoBundle 'itchyny/lightline.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'Valloric/YouCompleteMe'
NeoBundle 'edkolev/tmuxline.vim'
NeoBundle 'Valloric/YouCompleteMe', {
            \ 'build'      : {
            \ 'mac'     : './install.py --clang-completer --system-libclang'
            \ }
            \ }
"NeoBundle 'wting/rust.vim'
NeoBundle 'octol/vim-cpp-enhanced-highlight'
"Bundle 'hsanson/vim-android'

call neobundle#end()
"call vundle#end()

" Google things
nmap <C-g> :Google <c-r>=expand("%:e")<cr>

" Lightline config
" solarized
let g:lightline = {
            \ 'colorscheme': 'solarized_dark',
            \ 'active': {
            \   'left': [['mode'], ['filename'], ['line', 'percent']],
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

" Set the text for modes in lightline
function! LLMode()
    return
                \ lightline#mode() == 'NORMAL' ? 'N' :
                \ lightline#mode() == 'INSERT' ? 'I' :
                \ lightline#mode() == 'VISUAL' ? 'V' :
                \ lightline#mode() == 'V-LINE' ? 'V' :
                \ lightline#mode() == 'V-BLOCK' ? 'V' :
                \ lightline#mode() == 'REPLACE' ? 'R' : lightline#mode()
endfunction

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
let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
let g:ycm_filetype_blacklist = {}
let g:ycm_show_diagnostics_ui = 0
let g:ycm_cache_omnifunc = 0
" Makes youcompleteme not use the preview window
set completeopt=menuone
let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
"" Ultisnips
let g:UltiSnipsJumpForwardTrigger="<tab>"
nmap <C-s> <esc>mM :YcmCompleter GoTo<cr>

" Tmuxline
let g:tmuxline_powerline_separators = 0

let g:cpp_class_scope_highlight = 1

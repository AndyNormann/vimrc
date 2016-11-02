set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin()

Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'rking/ag.vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'szw/vim-g'
"Plug 'itchyny/lightline.vim'
Plug 'Shougo/unite.vim'
Plug 'scrooloose/nerdtree'
Plug 'flazz/vim-colorschemes'
Plug 'Shougo/context_filetype.vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'chriskempson/base16-vim'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'edkolev/tmuxline.vim'
Plug 'ElmCast/elm-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'jceb/vim-orgmode'
Plug 'vim-airline/vim-airline'
Plug 'bling/vim-bufferline'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline-themes'
"Plug 'Valloric/YouCompleteMe', {'do': './install.py --clang-completer --gocode-completer --racer-completer'}

call plug#end()

" Google things
"nmap <C-g> :Google <c-r>=expand("%:e")<cr> 
nmap <leader>g :Google <c-r>=expand("%:e")<cr> 

set background=dark
let g:airline_powerline_fonts = 1
let g:airline_theme='solarized'
let g:airline#extensions#default#layout = [
            \ ['a', 'b', 'c'],
            \ ['warning']]

" Unite config
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_grep_command = 'ag' "'ag --nocolor --nogroup -g'
let g:unite_source_line_enable_highlight = 1
let g:unite_force_overwrite_statusline = 0
let g:airline#extensions#branch#empty_message = ''

"" Unite binds
map <leader>b :Unite buffer<cr>
map <leader>f :Unite line -start-insert<cr>

"" YouCompleteMe
"let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
"let g:ycm_filetype_blacklist = {}
"let g:ycm_show_diagnostics_ui = 0
"let g:ycm_cache_omnifunc = 0
"" Makes youcompleteme not use the preview window
"set completeopt=menuone
"let g:ycm_key_list_select_completion = ['<C-j>', '<Down>']
"let g:ycm_key_list_previous_completion = ['<C-k>', '<Up>']
""" Ultisnips
"let g:UltiSnipsJumpForwardTrigger="<tab>"
"nmap <C-s> <esc>mM :YcmCompleter GoTo<cr>

"" NERDTree settings
let NERDTreeRespectWildIgnore=1
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=0
nmap \ :NERDTreeToggle<cr>

let g:cpp_class_scope_highlight = 1

" Ultisnips
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

" AsyncRun
noremap <leader>o :call asyncrun#quickfix_toggle(15)<cr>
noremap <leader>m :AsyncRun <Up>

" Bufferline
let g:bufferline_echo = 0
let g:bufferline_show_bufnr = 0
nmap <leader>l :bnext<cr>
nmap <leader>h :bprevious<cr>

"set background=dark
color blazer
"color colorful
"color solarized
"color seoul
"color grb256
"color railscasts
"color 256-jungle
"color mustang
"color wombat256i
"color molokai
"color gruvbox
"color hybrid
"color sourcerer
"color base16-eighties
"color base16-default-dark
"color smyck
"color lxvc
"color Tomorrow
"color Tomorrow-Night-Eighties

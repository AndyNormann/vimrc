set nocompatible              " be iMproved, required
filetype off                  " required

call plug#begin()

Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'rking/ag.vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'szw/vim-g'
Plug 'Shougo/unite.vim'
Plug 'scrooloose/nerdtree'
Plug 'flazz/vim-colorschemes'
Plug 'Shougo/context_filetype.vim'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdcommenter'
Plug 'ElmCast/elm-vim'
Plug 'skywind3000/asyncrun.vim'
Plug 'vim-airline/vim-airline'
Plug 'bling/vim-bufferline'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline-themes'
Plug 'morhetz/gruvbox'
Plug 'haya14busa/incsearch.vim'
Plug 'fatih/vim-go'
Plug 'xolox/vim-misc'
Plug 'xolox/vim-notes'
Plug 'Valloric/YouCompleteMe', {'do': './install.py --clang-completer --gocode-completer --racer-completer'}

call plug#end()

" Removes the --insert-- and so on
set noshowmode

" Google things
"nmap <C-g> :Google <c-r>=expand("%:e")<cr> 
nmap <leader>g :Google <c-r>=expand("%:e")<cr> 

set background=light
let g:airline_powerline_fonts = 1
"let g:airline_theme='solarized'
let g:airline_theme='base16_default'
"let g:airline_theme='gruvbox'
let g:airline#extensions#default#layout = [
            \ ['a', 'b', 'c'],
            \ ['warning']]

let g:airline_mode_map = {
            \ '__' : '-',
            \ 'n'  : 'N',
            \ 'i'  : 'I',
            \ 'R'  : 'R',
            \ 'c'  : 'C',
            \ 'v'  : 'V',
            \ 'V'  : 'V',
            \ '' : 'V',
            \ 's'  : 'S',
            \ 'S'  : 'S',
            \ '' : 'S',
            \ }

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
let g:UltiSnipsSnippetsDir="~/.vim/snippets"

" AsyncRun
noremap <silent><leader>o :call asyncrun#quickfix_toggle(15)<cr>
noremap <leader>m :AsyncRun <Up>
noremap <leader>r :AsyncRun 

" GoMode stuff
au FileType go nmap <Leader>[ <Plug>(go-run-split)

" Rust stuff
let g:ycm_rust_src_path = "/Users/andy_normann/.scripts/rust-master/src"
let g:rustc_path = "/Users/andy_normann/.cargo/bin/rustc"
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1


" Incsearc
set hlsearch
nmap / <Plug>(incsearch-forward)
nmap ? <Plug>(incserach-backward)
let g:incsearch#auto_nohlsearch = 1
nmap n <Plug>(incsearch-nohl-n)
nmap N <Plug>(incsearch-nohl-N)
nmap * <Plug>(incsearch-nohl-*)
nmap # <Plug>(incsearch-nohl-#)
nmap g* <Plug>(incsearch-nohl-g*)
nmap g# <Plug>(incsearch-nohl-g#)

" Bufferline
let g:bufferline_echo = 0
let g:bufferline_show_bufnr = 0
nmap <leader>l :bnext<cr>
nmap <leader>h :bprevious<cr>

" Elm 
let g:elm_make_output_file = "index.html"
let g:elm_jump_to_error = 1
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_detailed_complete = 1

" Neovim term
tnoremap <Esc> <C-\><C-n>

"set background=light
"color blazer
"color colorful
"color solarized
"color seoul
color grb256
"color railscasts
"color 256-jungle
"color mustang
"color wombat256i
"color molokai
"let g:gruvbox_contrast_dark = 'hard'
"color gruvbox
"color hybrid
"color sourcerer
"color base16-eighties
"color base16-default-dark
"color smyck
"color lxvc
"color Tomorrow
"color Tomorrow-Night-Eighties
"color Tomorrow-Night-Bright

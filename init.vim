
""""""""""""""""""
"                "
"  Plugin Stuff  "
"                "
""""""""""""""""""


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
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdtree'
Plug 'chriskempson/base16-vim'
Plug 'scrooloose/nerdcommenter'
Plug 'skywind3000/asyncrun.vim'
Plug 'itchyny/lightline.vim'
Plug 'cocopon/lightline-hybrid.vim'
Plug 'Raimondi/delimitMate'
Plug 'ElmCast/elm-vim'
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'rhysd/vim-clang-format'
Plug 'morhetz/gruvbox'
Plug 'cakebaker/scss-syntax.vim'
Plug 'hail2u/vim-css3-syntax'
Plug 'pangloss/vim-javascript'
Plug 'mattn/emmet-vim'
Plug 'maxmellon/vim-jsx-pretty'
Plug 'letientai299/vim-react-snippets', { 'branch': 'es6' }

call plug#end()

" Removes the --insert-- and so on
set noshowmode

" Google things
nmap <leader>g :Google <c-r>=expand("%:e")<cr> 

set background=dark

" Lightline
let g:lightline = {
            \ 'colorscheme': 'hybrid',
            \ 'active': { 
            \   'left': [ [ 'mode' ], [], ['filename', 'line', 'AsyncStatus'] ],
            \   'right': []
            \ },
            \ 'inactive': {
            \   'left': [['filename']],
            \   'right': []
            \ },
            \ 'component_function': {
            \   'filename': 'LightLineFilename',
            \   'AsyncStatus': 'MyAsyncRunStatus',
            \ }
            \}



let g:unite_force_overwrite_statusline = 0

function! LightLineReadonly()
    return &readonly ? '' : ''
endfunction

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

function! LightLineFilename()
    return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
                \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
                \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
endfunction

function! MyAsyncRunStatus()
  return
              \ g:asyncrun_status == 'success' ? "\u2714" : 
              \ g:asyncrun_status == 'failure' ? "\u2718" :
              \ "-"
endfunction


" Unite config
let g:unite_split_rule = "botright"
let g:unite_enable_short_source_names = 1
let g:unite_source_grep_command = 'rg' 
let g:unite_source_line_enable_highlight = 1
let g:unite_force_overwrite_statusline = 0

"" Unite binds
noremap <leader>b :Unite buffer<cr>
noremap <leader>f :Unite line -start-insert<cr>
noremap <leader>e :Unite file<cr>

let g:cpp_class_scope_highlight = 1

" Ultisnips
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetsDir="~/.vim/snippets"

" AsyncRun
noremap <silent><leader>o :call asyncrun#quickfix_toggle(10)<cr>
noremap <leader>m :AsyncRun <Up>
noremap <leader>r :AsyncRun 

" GoMode stuff
let g:go_fmt_autosave = 1
let g:go_fmt_fail_silently = 1
let g:go_metalinter_autosave = 1

" C stuff
let g:clang_format#code_style = 'WebKit'
let g:deoplete#sources#clang#libclang_path = '/usr/local/Cellar/llvm/3.9.1/lib/libclang.dylib'
let g:deoplete#sources#clang#clang_header = '/usr/local/Cellar/llvm/3.9.1/lib/clang'

" Rust stuff
let g:deoplete#sources#rust#racer_binary='/Users/andreasnormann/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path='/Users/andreasnormann/.scripts/rust/src'
let g:ycm_rust_src_path = "/Users/andreasnormann/.scripts/rust/src"
let g:rustc_path = "/Users/andreasnormann/.cargo/bin/rustc"
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1

" Elm 
let g:elm_make_output_file = "main.js"
let g:elm_jump_to_error = 1
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_detailed_complete = 1
let g:elm_setup_keybindings = 0

"color blazer
"color colorful
"color solarized
"color seoul
"color grb256
"color railscasts
"color 256-jungle
"color mustang
"color wombat256i
"color molokai
let g:gruvbox_contrast_dark = 'hard'
color gruvbox
"color hybrid
"color sourcerer
"color base16-eighties
"color base16-default-dark
"color base16-onedark
"color smyck
"color lxvc
"color Tomorrow
"color Tomorrow-Night-Eighties
"color Tomorrow-Night-Bright


"""""""""""""""""
"               "
"  Basic Stuff  "
"               "
"""""""""""""""""

"""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""
" Sets how many lines vim will remember
set history=700

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

""""""""""""""""""""""""""
" => Leader hotkeys
"""""""""""""""""""""""""
" Saving and quiting
noremap <leader>w :w<cr>
noremap <leader>q :bdelete<cr>

noremap <silent> <leader><cr> :noh<cr>

"""""""""""""""""""""""""""""
" => User interface
"""""""""""""""""""""""""""""
set inccommand=split
"  Set mouse mode
set mouse=a

" Turn on wild menu
set wildmenu

" Make wild menu ignore compiled files
set wildignore=*.o,*~,*.class

" Height of the command bar
set cmdheight=1
set cmdwinheight=1

" Configure backspace so it acts like backspace
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like saerch in modern browsers
set incsearch

" For regular expressions turn magic on
set magic

" Fix escape delay in normal vim
set timeoutlen=1000 ttimeoutlen=0

" Show matching brackets
set showmatch
" How many tenths of a second to blink when matching brackets
set mat=2

" Remove annoying error sounds
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Line numbers
"set number

" set 0 to jump to last non whitespace character
map 0 ^

" Show statusline
set laststatus=2

" Turn backup off, learn to use git
set nobackup
set nowb
set noswapfile

" Turn off mode output on the bottom
set nosmd
set noru

" No more lastline
set display=lastline

""""""""""""""""""""""""""
" => Colors and Fonts
""""""""""""""""""""""""""
" Syntax highlighting
syntax enable

" Color
set t_Co=256
set termguicolors

" Set utf8 encoding
set encoding=utf8

" Use unix as standard filetype
set ffs=unix,dos,mac


"""""""""""""""""""""""""""
" => Text, tabs and indent
"""""""""""""""""""""""""""
" Spaces instead of tabs
set expandtab

" Set smarttab
set smarttab

" Set tabwidth
set shiftwidth=4
set tabstop=4

" Linebreak on 500 characters
set lbr
set tw=500

set ai "auto indent
set si "smart indent
set wrap "wrap lines

" Return to last edit position when opening files
autocmd BufReadPost *
            \ if line("'\"") > 0 && line("'\"") <= line("$") |
            \   exe "normal! g`\"" |
            \ endif
" Remember info about open buffers on close
set viminfo^=%

" Makes all copy and paste go to system clipboard
set clipboard=unnamed


"""""""""""""""""""""""""""""
" => Helper Functions
""""""""""""""""""""""""""""
" Deletes trailing whitespace
func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal `z"
endfunc

""""""""""""""""""""""""""""
" => Templates
""""""""""""""""""""""""""""
augroup templates
    au!
    autocmd BufNewFile *.* silent! execute '0r ~/.vim/templates/template.'.expand("<afile>:e")
augroup END

function! NewJavaFile()
    %s/FILE/\=expand("%:t:r")/g
endfunction

autocmd BufNewFile *.java call NewJavaFile()

""""""""""""""""""""""""""
" Persistent undo
" so you can undo even after closing a buffer/VIM
""""""""""""""""""""""""""
try
    set undodir=~/.vim/temp_dirs/undodir
    set undofile
catch
endtry

""""""""""""""""""""""""""
" => Parenthesis/bracket
""""""""""""""""""""""""""
" Map auto complete of (, ", ', [
inoremap $1 ()<esc>i
inoremap $2 []<esc>i
inoremap $3 {}<esc>i
inoremap $4 {<esc>o}<esc>O
inoremap $q ''<esc>i
inoremap $e ""<esc>i
inoremap $t <><esc>i

""""""""""""""""""""""""
" => Movement
"""""""""""""""""""""""
nmap } 5gj
nmap { 5gk

noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj
noremap <buffer> <silent> 0 g0
noremap <buffer> <silent> $ g$

nnoremap ; :
nnoremap : ;


""""""""""""""""""""
"                  "
"  Filetype stuff  "
"                  "
""""""""""""""""""""

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
au FileType c nnoremap <Leader>t :!/tmp/./%< 
au FileType c nnoremap <Leader>c :AsyncRun clang -Weverything -o /tmp/%:t:r %:t
au FileType c inoremap <Leader>. ->

""""""""""""""""""""""""""""
" => Cpp section
"""""""""""""""""""""""""""""
au FileType cpp nnoremap <Leader>t :!/tmp/%<
au FileType cpp nnoremap <Leader>c :AsyncRun clang++ -Weverything -std=c++14 -o /tmp/%:t:r %
au FileType cpp inoremap ,. ->
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

"""""""""""""""""""""""""""""
" => Rust section
""""""""""""""""""""""""""""""
au FileType rust inoremap ., ::
au FileType rust nnoremap <Leader>c :AsyncRun cargo build
au FileType rust nnoremap <Leader>t :AsyncRun cargo run


au BufRead,BufNewFile *.hbs setfiletype html

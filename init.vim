""""""""""""""""""
"                "
"  Plugin Stuff  "
"                "
""""""""""""""""""
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"


set nocompatible              " be iMproved, required
filetype off                  " required 
call plug#begin()

Plug 'honza/vim-snippets'
Plug 'SirVer/ultisnips'
Plug 'Shougo/denite.nvim'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdcommenter'
Plug 'skywind3000/asyncrun.vim'
Plug 'Raimondi/delimitMate'
Plug 'sheerun/vim-polyglot'
Plug 'w0ng/vim-hybrid'
Plug 'tpope/vim-fugitive'
Plug 'morhetz/gruvbox'

call plug#end()

let g:gruvbox_contrast_dark = 'hard'

" Denite config
call denite#custom#var('grep', 'command', ['rg'])
call denite#custom#var('grep', 'default_opts',
            \ ['--vimgrep', '--no-heading'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])

" Denite binds
nnoremap <leader>b :Denite buffer -mode=normal<cr>
nnoremap <leader>f :Denite line auto-highlight<cr>
nnoremap <leader>e :e .<cr>

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

" Fugitive
nnoremap <leader>g :Gstatus<cr>

" GoMode stuff
let g:go_fmt_autosave = 1
let g:go_fmt_fail_silently = 1

" Rust stuff
let g:ycm_rust_src_path = "/Users/andreasnormann/.scripts/rust/src"
let g:rustc_path = "/Users/andreasnormann/.cargo/bin/rustc"
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1

" Elm 
let g:elm_make_output_file = "main.js"
let g:elm_format_autosave = 1
let g:elm_format_fail_silently = 1
let g:elm_setup_keybindings = 0


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
set background=dark
"color hybrid
color gruvbox

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

""""""""""""""""""""""""
" => Movement
"""""""""""""""""""""""
noremap <buffer> <silent> k gk
noremap <buffer> <silent> j gj
noremap <buffer> <silent> 0 g0
noremap <buffer> <silent> $ g$

nnoremap ; :
nnoremap : ;

noremap <C-u> 7<C-u>
noremap <C-d> 7<C-d>


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



""""""""""""""""
"              "
"  Statusline  "
"              "
""""""""""""""""

let g:currentmode={
            \ 'n'  : 'NORMAL ',
            \ 'no' : 'OPERATOR ',
            \ 'v'  : 'VISUAL ',
            \ 'V'  : 'VISUAL-L ',
            \ '' : 'VISUAL-B ',
            \ 's'  : 'Select ',
            \ 'S'  : 'S-Line ',
            \ '' : 'S-Block ',
            \ 'i'  : 'INSERT ',
            \ 'R'  : 'REPLACE ',
            \ 'Rv' : 'V·Replace ',
            \ 'c'  : 'Command ',
            \ 'cv' : 'Vim Ex ',
            \ 'ce' : 'Ex ',
            \ 'r'  : 'Prompt ',
            \ 'rm' : 'More ',
            \ 'r?' : 'Confirm ',
            \ '!'  : 'Shell ',
            \ 't'  : 'Terminal '
            \}

function! ReadOnly()
    if &readonly || !&modifiable
        return ''
    else
        return ''
    endif
endfunction

function! MyAsyncRunStatus()
    return
                \ g:asyncrun_status == 'success' ? "\u002b" : 
                \ g:asyncrun_status == 'failure' ? "\u00d7" :
                \ "\u2010"
endfunction

set laststatus=2
set statusline=
set statusline+=%0*\ %{g:currentmode[mode()]}             " Current mode
set statusline+=%8*\ %l\                                  " Rownumber/total (%)
set statusline+=%{MyAsyncRunStatus()}                     " AsyncRun status
set statusline+=%8*\ %<%f\ %{ReadOnly()}\ %m\ %w\         " File+path

"hi StatusLineNC guifg=#707880 guibg=#1a1d10 gui=none cterm=none 
hi StatusLine guifg=#282828 guibg=#ebdbb2 gui=none cterm=none 

set noshowmode

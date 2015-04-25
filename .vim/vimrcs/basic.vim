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

" Set leader key
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

""""""""""""""""""""""""""
" => Leader hotkeys
"""""""""""""""""""""""""
" Saving and quiting
nmap <leader>w :w<cr>
nmap <leader>q :bdelete<cr>
nmap <leader>x :qa<cr>

map <silent> <leader><cr> :noh<cr>
" Make hotkeys
noremap <Leader>m :!clear && make<CR>
noremap <Leader>r :!clear && make run<CR>
" Git hotkeys
noremap <C-c> :!git commit -m ""
noremap <C-a> :!git add
noremap <C-s> :!git push<CR>
" Copy and paste to/from system clipboard
vmap <leader>y "+y
vmap <leader>d "+d
nmap <leader>p "+p
" Move between buffers
nmap <leader>b :bnext<cr>
nmap <leader>v :bprevious<cr>
" Delete trailing whitespace
nmap <leader>ws :call DeleteTrailingWS()<cr>


"""""""""""""""""""""""""""""
" => User interface
"""""""""""""""""""""""""""""
" Turn on wild menu
set wildmenu

" Make wild menu ignore compiled files
set wildignore=*.o,*~,*.class

" Height of the command bar
set cmdheight=1

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
set number

" set 0 to jump to last non whitespace character
map 0 ^

" Show statusline
set laststatus=2

" Turn backup off, learn to use git
set nobackup
set nowb
set noswapfile

""""""""""""""""""""""""""
" => Colors and Fonts
""""""""""""""""""""""""""
" Syntax highlighting
syntax enable

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

"""""""""""""""""""""""""""""
" => Helper Functions
""""""""""""""""""""""""""""
" Returns true if paste mode is enabled
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
    s/FILE/\=expand("%:t:r")
endfunction

autocmd BufNewFile *.java call NewJavaFile()

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
let mapleader = ","
let g:mapleader = ","

""""""""""""""""""""""""""
" => Leader hotkeys
"""""""""""""""""""""""""
" Saving and quiting
nmap <leader>w :w<cr>
nmap <leader>q :bdelete<cr>
nmap <leader>x :qa<cr>
nmap <leader>e :e 

map <silent> <leader><cr> :noh<cr>
" Make hotkeys
noremap <Leader>m :!clear && make<CR>
noremap <Leader>r :!clear && make run<CR>
" Move between buffers(Replaced by unite)
"nmap <C-l> :bnext!<cr>
"nmap <C-h> :bprevious!<cr>
" Delete trailing whitespace
nmap <leader>` :call DeleteTrailingWS()<cr>


"""""""""""""""""""""""""""""
" => User interface
"""""""""""""""""""""""""""""
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

" Hightlight current line
set cursorline
hi CursorLine term=none cterm=none ctermbg=Magenta 
" DarkRed could work

" Turn backup off, learn to use git
set nobackup
set nowb
set noswapfile

" Turn off mode output on the bottom
set nosmd
set noru

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
    s/FILE/\=expand("%:t:r")
endfunction

autocmd BufNewFile *.java call NewJavaFile()

""""""""""""""""""""""""""
" Persistent undo
" so you can undo even after closing a buffer/VIM
""""""""""""""""""""""""""
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
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
nmap <C-j> <C-d>zz
nmap <C-k> <C-u>zz
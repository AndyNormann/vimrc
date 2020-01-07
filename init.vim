""""""""""""""""""
"                "
"  Plugin Stuff  "
"                "
""""""""""""""""""
let mapleader = "\<Space>"
let g:mapleader = "\<Space>"

set nocompatible              " be iMproved, required
filetype off                  " required 

set runtimepath+=~/.config/nvim/repos/github.com/Shougo/dein.vim

call dein#begin('~/.config/nvim')

call dein#add('Shougo/dein.vim')
call dein#add('sonph/onehalf', {'rtp': 'vim'})
call dein#add('kyoz/purify', {'rtp': 'vim'})
call dein#add('chase/focuspoint-vim')
call dein#add('AlessandroYorba/Alduin')
call dein#add('ajmwagar/vim-deus')
call dein#add('mhartington/oceanic-next')
call dein#add('rakr/vim-two-firewatch')
call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })
call dein#add('junegunn/fzf.vim')
call dein#add('easymotion/vim-easymotion')
call dein#add('honza/vim-snippets')
call dein#add('SirVer/ultisnips')
call dein#add('tpope/vim-surround')
call dein#add('junegunn/vim-easy-align')
call dein#add('tpope/vim-commentary')
call dein#add('kassio/neoterm')
call dein#add('Raimondi/delimitMate')
call dein#add('sheerun/vim-polyglot')
call dein#add('evanleck/vim-svelte')
call dein#add('honza/vim-snippets')
call dein#add('tpope/vim-vinegar')
call dein#add('mattn/emmet-vim')
call dein#add('tpope/vim-fugitive')
call dein#add('itchyny/lightline.vim')
call dein#add('neoclide/coc.nvim', {'merged':0, 'rev': 'release'})
call dein#add('sbdchd/neoformat')
call dein#add('prettier/vim-prettier', {'do': 'yarn install', 'for': ['typescript', 'json', 'graphql']})

call dein#end()

" FZF 
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Rg<cr>
nnoremap <leader>e :Files<cr>
nnoremap <leader>l :Marks<cr>

command! -bang -nargs=* Rg
            \ call fzf#vim#grep(
            \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
            \   <bang>0 ? fzf#vim#with_preview('up:60%')
            \           : fzf#vim#with_preview('right:50%:hidden', '?'),
            \   <bang>0)

" Ultisnips
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetsDir="~/.config/nvim/snippets"

" Easy align
xmap ga <Plug>(EasyAlign)

" Easymotion
nmap <silent>f <Plug>(easymotion-jumptoanywhere)

" Coc
function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

" Lightline
let g:lightline = {
            \ 'colorscheme': 'purify',
            \ 'active': {
            \   'left': [ [ 'mode' ], [ 'gitbranch', 'cocstatus', 'currentfunction' ], [ 'readonly', 'filename', 'modified', 'line' ] ],
            \  'right': [[]],
            \ },
            \ 'inactive': {
            \   'left': [ ['filename', 'readonly', 'modified', 'line'] ],
            \   'right': [[]],
            \ },
            \ 'component_function': {
            \   'cocstatus': 'coc#status',
            \   'currentfunction': 'CocCurrentFunction',
            \   'gitbranch': 'fugitive#head'
            \ }
            \ }



" Prettier
autocmd BufWritePre *.json,*.graphql Neoformat
let g:neoformat_enabled_javascript = ['prettier']
let g:neoformat_only_msg_on_error = 1

let g:neoformat_basic_format_align = 1

" Align
xmap ga <Plug>(EasyAlign)

" Fugitive
nnoremap <leader>g :Gstatus<cr>

" GoMode stuff
let g:go_fmt_autosave = 1
let g:go_fmt_fail_silently = 1


"""""""""""""""""
"               "
"  Basic Stuff  "
"               "
"""""""""""""""""

"""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""
" Save
nnoremap s :w<cr>
nnoremap Q :qa<cr>
" Sets how many lines vim will remember
set history=700

set guicursor=

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

""""""""""""""""""""""""""
" => Leader hotkeys
"""""""""""""""""""""""""
" Saving and quiting
noremap <silent> <leader><cr> :noh<cr>

"""""""""""""""""""""""""""""
" => User interface
"""""""""""""""""""""""""""""
set inccommand=split
"  Set mouse mode
set mouse=a

" Disable startup screen
set shortmess=I

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
set number

" set 0 to jump to last non whitespace character
map 0 ^

nmap <silent> j gj
nmap <silent> k gk

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
set background=light
color onehalfdark

highlight Comment gui=italic cterm=italic
highlight htmlArg gui=italic cterm=italic
highlight Special gui=italic cterm=italic

"""""""""""""""""""""""""""
" => Text, tabs and indent
"""""""""""""""""""""""""""
" Spaces instead of tabs
set expandtab

" Set smarttab
set smarttab

" Set tabwidth
set shiftwidth=2
set tabstop=2

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
set clipboard=unnamedplus

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

noremap { 7gk
noremap } 7gj


""""""""""""""""""""
"                  "
"  Filetype stuff  "
"                  "
""""""""""""""""""""

"""""""""""""""""""""""
" => Python
"""""""""""""""""""""""
au FileType python nnoremap <leader>r :!python3 %<cr>
" au FileType python nnoremap <leader>t :!python3 % 

"""""""""""""""""""""""
" => Ruby
"""""""""""""""""""""""
au FileType ruby nnoremap <leader>r :!ruby %<cr>
" au FileType ruby nnoremap <leader>t :!ruby % 

"""""""""""""""""""""""
" => Elm
"""""""""""""""""""""""
au FileType elm nnoremap <leader>c :ElmMake<cr>

"""""""""""""""""""""""""""""
" => C section
""""""""""""""""""""""""""""""
" au FileType c nnoremap <Leader>t :!/tmp/./%< 
au FileType c nnoremap <Leader>c :AsyncRun clang -Weverything -o /tmp/%:t:r %:t
au FileType c inoremap ,. ->

""""""""""""""""""""""""""""
" => Cpp section
"""""""""""""""""""""""""""""
" au FileType cpp nnoremap <Leader>t :!/tmp/%<
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
au FileType go inoremap ,. :=

"""""""""""""""""""""""""""""
" => Rust section
""""""""""""""""""""""""""""""
au FileType rust inoremap ., ::
au FileType rust nnoremap <Leader>c :AsyncRun cargo build
au FileType rust nnoremap <Leader>t :AsyncRun cargo run


"""""""""""""""""""""""""""
" => Onivim section
"""""""""""""""""""""""""""

if exists('g:gui_oni')
  set noshowmode
  set nonumber
  set laststatus=0
endif

"""""""""""""""""""""""""""
" => Term section
"""""""""""""""""""""""""""
tnoremap <esc><esc> <C-\><C-n>
let g:neoterm_autoscroll=1
let g:neoterm_default_mod='vertical rightbelow 35%'

nnoremap <silent><leader>o :<c-u>exec v:count.'Ttoggle'<cr>
nnoremap <leader>r :T 
nnoremap <leader>t :Ttoggle<cr>
nnoremap <leader>m :T <Up>

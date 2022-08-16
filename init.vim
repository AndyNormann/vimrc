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
set runtimepath+=~/.config/nvim/autoload

call dein#begin('~/.config/nvim')

call dein#add('Shougo/dein.vim')
call dein#add('sonph/onehalf', {'rtp': 'vim'})
call dein#add('huyvohcmc/atlas.vim')
call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })
call dein#add('junegunn/fzf.vim')
call dein#add('tpope/vim-surround')
call dein#add('tpope/vim-commentary')
call dein#add('mattn/emmet-vim')
call dein#add('itchyny/lightline.vim')
call dein#add('honza/vim-snippets')
call dein#add('voldikss/vim-floaterm')
call dein#add('pangloss/vim-javascript')
call dein#add('MaxMEllon/vim-jsx-pretty')
call dein#add('neoclide/vim-jsx-improve')
call dein#add('leafOfTree/vim-svelte-plugin')
call dein#add('HerringtonDarkholme/yats.vim')
call dein#add('udalov/kotlin-vim.git')
call dein#add('windwp/nvim-autopairs')
call dein#add('SirVer/ultisnips')
call dein#add('neoclide/coc.nvim', {'merged':0, 'rev': 'release'})
call dein#add('rieg-ec/coc-tailwindcss', {'do': 'yarn install --frozen-lockfile && yarn run build'})
call dein#add('prettier/vim-prettier', {'build': 'npm install', 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'svelte', 'yaml', 'html']})

call dein#end()

" FZF 
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>f :Rg<cr>
nnoremap <leader>F :Rg<cr><C-P>
nnoremap <leader>e :Files<cr>
nnoremap <leader>E :Files<cr><C-P>

let g:fzf_history_dir="~/.config/nvim/fzf-history"


command! -bang -nargs=* Rg
            \ call fzf#vim#grep(
            \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
            \   <bang>0 ? fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'up:60%')
            \           : fzf#vim#with_preview({'options': '--delimiter : --nth 4..'}, 'right:50%:hidden', '?'),
            \   <bang>0)

" Ultisnips
inoremap <silent><expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
let g:UltiSnipsSnippetsDir="~/.config/nvim/snippets"

" Floatterm
let g:floaterm_wintype="split"
let g:floaterm_height=0.25
let g:floaterm_position="belowright"
nnoremap <leader>d :FloatermToggle<cr><C-\><C-n>
nnoremap <leader>D :FloatermNew<cr>

" Autopairs
lua require('nvim-autopairs').setup()

"" Coc
" Set tab for snippet completion
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)


" use <c-space>for trigger completion
inoremap <silent><expr> <c-f> coc#refresh()

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

function! CocGitDiff()
  return get(b:, 'coc_git_status', '')
endfunction

function! CocLastHover()
  return get(g:, 'coc_last_hover_message', '')
endfunction

function! CocDiagnostic()
  return get(b:, 'coc_diagnostic_info', '')['error']
endfunction

function! CocGitStatus()
  return get(g:, 'coc_git_status', '')
endfunction

let g:jsx_ext_required = 0 

" Lightline
let g:lightline = {
      \   'colorscheme': 'onehalfdark',
      \   'active': {
      \     'left': [ [ 'mode' ], 
      \             [ 'cocstatus' ],
      \             [ 'readonly', 
      \               'filename', 
      \               'modified', 
      \               'line' ] 
      \             ],
      \    'right': [[]],
      \   },
      \   'inactive': {
      \     'left': [ ['filename', 'readonly', 'modified', 'line'] ],
      \     'right': [[]],
      \   },
      \   'component_function': {
      \     'cocstatus': 'coc#status',
      \   }
      \ }

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

set signcolumn=yes
set guicursor=

" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread

""""""""""""""""""""""""""
" => Leader hotkeys
"""""""""""""""""""""""""
noremap <silent> <leader><cr> :noh<cr>

"""""""""""""""""""""""""""""
" => User interface
"""""""""""""""""""""""""""""
set inccommand=split
"  Set mouse mode
set mouse=a

" Disable startup screen
set shortmess+=I
" set shortmess+=ca
set hidden

" Help for coc vim
set updatetime=300
set cmdheight=2

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
set nonumber

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
set background=dark
" color vimspectr210-light
color onehalfdark

" color yowish
" color two-firewatch

" let g:yowish.colors = {
"       \	'backgroundLight'    : ['#0c0e12', '232']
"       \ }
" let g:yowish = {
" 			\ 'term_italic': 1,
" 		\ }

highlight Normal guibg=#1d232e
highlight SignColumn guibg=#1d232e

" highlight Normal guibg=#0c0e12
" highlight SignColumn guibg=#0c0e12
" highlight SignColumn guibg=#2C323B
highlight Comment gui=italic cterm=italic
highlight htmlArg gui=italic cterm=italic
" highlight jsxAttrib guifg=#ffcc66
highlight Special gui=italic cterm=italic
highlight Character gui=italic cterm=italic

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
" au FileType elm nnoremap <leader>c :ElmMake<cr>

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
au FileType cpp nnoremap <Leader>c :FloatermSend clang++ -std=c++14 -o /tmp/%:t:r %
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

" let g:floaterm_keymap_toggle = '<f5>'


nnoremap <silent><leader>o :<c-u>exec v:count.'FloatermToggle'<cr><C-\><C-n>
nnoremap <leader>r :FloatermSend 
nnoremap <leader>m :FloatermSend<Up>

set t_md=

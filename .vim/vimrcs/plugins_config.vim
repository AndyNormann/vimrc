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
Plug 'chriskempson/base16-vim'
Plug 'scrooloose/nerdcommenter'
Plug 'skywind3000/asyncrun.vim'
Plug 'itchyny/lightline.vim'
Plug 'cocopon/lightline-hybrid.vim'
Plug 'tpope/vim-fugitive'
Plug 'Raimondi/delimitMate'
Plug 'ElmCast/elm-vim'
Plug 'fatih/vim-go'
Plug 'jodosha/vim-godebug'
Plug 'rust-lang/rust.vim'
Plug 'wookiehangover/jshint.vim'
Plug 'rhysd/vim-clang-format'
"Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'zchee/deoplete-clang'
"Plug 'sebastianmarkow/deoplete-rust'

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
            \   'left': [ [ 'mode' ], ['fugitive'], ['filename', 'line', 'AsyncStatus'] ],
            \   'right': []
            \ },
            \ 'inactive': {
            \   'left': [['filename']],
            \   'right': []
            \ },
            \ 'component_function': {
            \   'readonly': 'MyReadonly',
            \   'filename': 'LightLineFilename',
            \   'AsyncStatus': 'MyAsyncRunStatus',
            \   'fugitive': 'MyFugitive'
            \ }
            \}



let g:unite_force_overwrite_statusline = 0


function! MyFugitive()
  try
    if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
      let _ = fugitive#head()
      return strlen(_) ? "\ue0a0 "._ : ''
    endif
  catch
  endtry
  return ''
endfunction

" Sets a fancy symbol if the file is readonly
function! LightLineReadonly()
    if &filetype == "help"
        return ""
    elseif &readonly
        return ""
    else
        return ""
    endif
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
let g:unite_source_grep_command = 'ag' "'ag --nocolor --nogroup -g'
let g:unite_source_line_enable_highlight = 1
let g:unite_force_overwrite_statusline = 0

"" Unite binds
map <leader>b :Unite buffer<cr>
map <leader>f :Unite line -start-insert<cr>

" Deoplete
let g:deoplete#enable_at_startup = 1
let g:deoplete#auto_complete_start_length = 1
let g:deoplete#auto_complete_delay = 50
let g:deoplete#sources#go#gocode_binary = $GOPATH.'/bin/gocode'
set completeopt-=preview


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
noremap <silent><leader>o :call asyncrun#quickfix_toggle(10)<cr>
noremap <leader>m :AsyncRun <Up>
noremap <leader>r :AsyncRun 

" GoMode stuff
let g:go_fmt_autosave = 1
let g:go_fmt_fail_silently = 1
let g:go_highlight_operators = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
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
"color grb256
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
color base16-default-dark
"color base16-onedark
"color smyck
"color lxvc
"color Tomorrow
"color Tomorrow-Night-Eighties
"color Tomorrow-Night-Bright

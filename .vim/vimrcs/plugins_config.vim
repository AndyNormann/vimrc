set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Bundle 'gmarik/Vundle.vim'

Bundle 'flazz/vim-colorschemes'
Bundle 'itchyny/lightline.vim'
Bundle 'honza/vim-snippets'
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "garbas/vim-snipmate"
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'junkblocker/unite-codesearch'
Bundle 'Shougo/neomru.vim'
Bundle 'ujihisa/unite-colorscheme'
Bundle 'Shougo/unite-outline'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/nerdtree'
Bundle 'bling/vim-bufferline'

call vundle#end()

let g:unite_force_overwrite_statusline = 0

let g:lightline = {
            \ 'colorscheme': 'Tomorrow_Night',
            \ 'active': {
            \   'left': [ [ 'mode', 'paste' ], ['buffer'], ['bufferline'] ],
            \   'right': []
            \ },
            \ 'inactive': {
            \   'left': [['readonly'], ['filename'], ['modified']],
            \   'right': []
            \ },
            \ 'component': {
            \     'bufferline': '%{bufferline#refresh_status()}%{MyBufferline()[0]}'.
            \                   '%#TabLineSel#%{g:bufferline_status_info.current}'.
            \                   '%#LightLineLeft_active_2#%{MyBufferline()[2]}',
            \     'buffer': 'Buffers'
            \ },
            \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
            \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" },
            \ 'component_function': {
            \   'mode': 'LLMode',
            \   'readonly': 'MyReadonly',
            \   'filename': 'MyFilename'
            \ }
            \}

" Function for setting the text for modes in lightline
function! LLMode()
  return
        \ lightline#mode() == 'NORMAL' ? 'N' :
        \ lightline#mode() == 'INSERT' ? 'I' :
        \ lightline#mode() == 'VISUAL' ? 'V' :
        \ lightline#mode() == 'V-LINE' ? 'V' :
        \ lightline#mode() == 'V-BLOCK' ? 'V' :
        \ lightline#mode() == 'REPLACE' ? 'R' : lightline#mode()
endfunction

function! MyReadonly()
    return &readonly ? '' : ''
endfunction

function! MyBufferline()
    call bufferline#refresh_status()
    let b = g:bufferline_status_info.before
    let c = g:bufferline_status_info.current
    let a = g:bufferline_status_info.after
    let alen = strlen(a)
    let blen = strlen(b)
    let clen = strlen(c)
    let w = winwidth(0) * 4 / 10
    if w < alen+blen+clen
        let whalf = (w - strlen(c)) / 2
        let aa = alen > whalf && blen > whalf ? a[:whalf] : alen + blen < w - clen || alen < whalf ? a : a[:(w - clen - blen)]
        let bb = alen > whalf && blen > whalf ? b[-(whalf):] : alen + blen < w - clen || blen < whalf ? b : b[-(w - clen - alen):]
        return [(strlen(bb) < strlen(b) ? '...' : '') . bb, c, aa . (strlen(aa) < strlen(a) ? '...' : '')]
    else
        return [b, c, a]
    endif
endfunction

" Bufferline config
let g:bufferline_echo = 0
let g:bufferline_active_buffer_left = ""
let g:bufferline_active_buffer_right = ""
let g:bufferline_show_bufnr = 0
let g:bufferline_fname_mod = ':t'


" Unite config
call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_cursor_line_highlight = 'CursorLine'
let g:unite_source_codesearch_command = '/usr/local/bin/csearch'
let g:unite_split_rule = "botright"
let g:unite_winheight = 10
let g:unite_enable_short_source_names = 1
let g:unite_source_file_mru_filename_format = ':~:.'
let g:unite_source_file_mru_time_format = ''
let g:unite_source_grep_command = 'ag'

let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

nnoremap ? :Unite -start-insert -auto-resize file file_rec/async file_mru<CR>
nnoremap / :Unite -start-insert -auto-resize line<cr>
nnoremap <silent><c-n> :<C-u>UniteResume<CR>

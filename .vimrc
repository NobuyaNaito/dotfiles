if !&compatible
  set nocompatible
endif

"===============================================================================
"===============================================================================
" Plugins.

let s:plugin_dir = $HOME . "/.vim/bundles"
let s:dein_dir = s:plugin_dir . "/repos/github.com/Shougo/dein.vim"

" If dein.vim is installed.
if isdirectory(s:dein_dir)

  let &runtimepath = s:dein_dir .",". &runtimepath

  if dein#load_state(s:plugin_dir)
    call dein#begin(s:plugin_dir)
    call dein#load_toml(s:plugin_dir . "/dein.toml",      {"lazy": 0})
    call dein#load_toml(s:plugin_dir . "/dein_lazy.toml", {"lazy": 1})
    call dein#end()
    call dein#save_state()
  endif

  " Install not installed plugins on startup.
  if dein#check_install()
    call dein#install()
  endif

  " Update plugins.
  "call dein#update()

  " Uninstall non-used plugins.
  "call map(dein#check_clean(),"delete(v:val,'rf')")
  "call dein#recache_runtimepath()

endif

"===============================================================================
"===============================================================================
" Other settings.

filetype on
filetype plugin on
filetype indent off
syntax enable

" runtimepath
let s:dotfiles_runtime = $HOME . "/dotfiles/vimruntime"
let &runtimepath = s:dotfiles_runtime .",". &runtimepath
let &runtimepath = &runtimepath .",". s:dotfiles_runtime . "/after"

" mapping
noremap <S-h> ^
noremap <S-l> $
nnoremap <C-h> gT
nnoremap <C-l> gt
"nnoremap :te :tabedit
vnoremap * "zy:let @/ = @z<CR>n
" instant escape from insert and visual mode
inoremap <silent> <ESC> <ESC>:<CR>
vnoremap <silent> <ESC> <ESC>:<CR>

" search
nnoremap <ESC><ESC> :set hlsearch!<CR>
nnoremap / :set hlsearch<CR>/
nnoremap ? :set hlsearch<CR>?
nnoremap * :set hlsearch<CR>*
nnoremap # :set hlsearch<CR>#
nnoremap g* :set hlsearch<CR>g*
nnoremap g# :set hlsearch<CR>g#
set hlsearch
set incsearch
set ignorecase
set smartcase

" indent
set autoindent

" tab
set expandtab
set smarttab
set tabstop=2
set shiftwidth=2
set softtabstop=2

" statusline
set laststatus=2
set ruler

" tabline
set showtabline=2

" column
"execute "set colorcolumn=" . join(range(81, 200), ',')

" other setting
set scrolloff=4
set whichwrap=b,s,<,>,[,]
set number
set relativenumber
set cursorline
"set formatoptions-=or   => &runtimepath /after/plugin/common_settings.vim
set formatoptions=q
set backspace=indent,eol,start

augroup QuickFixOpen
  autocmd!
  autocmd QuickFixCmdPost make,*grep* cwindow
augroup END

augroup QuickFixVerticalRight
  autocmd!
  autocmd FileType qf wincmd L
augroup END

augroup HelpVerticalRight
  autocmd!
  autocmd FileType help wincmd L
augroup END

" Uppercase/lowercase adjustment is enabled in ins completion.
" 'set ignorecase' is required.
set infercase

" Options for ins completion.
set completeopt=menu,menuone,preview

augroup OmniFuncAll
  autocmd!
  autocmd FileType * 
  \ if &l:omnifunc == ""
  \ |   setlocal omnifunc=syntaxcomplete#Complete
  \ | endif
augroup END

" Stop completion when editing completed text.
inoremap <expr> <BS> pumvisible() ? "\<C-y>\<BS>" : "\<BS>"

"===============================================================================
"===============================================================================
" Insert mode mappings.

" Insert '%'.
inoremap <silent><expr> <C-p> pumvisible() ? "\<C-p>" : "%"

" Linebreak for Fortran.
inoremap <silent> <C-f><CR> <Space>&<CR>&<Space>

" Insert character toward the 80th column.
inoremap <silent> <C-f>- <C-\><C-o>:call <SID>InsertComment80("-")<CR><Right>
inoremap <silent> <C-f>= <C-\><C-o>:call <SID>InsertComment80("=")<CR><Right>
inoremap <silent> <C-f>! <C-\><C-o>:call <SID>InsertComment80("!")<CR><Right>

" Fill from cursor position to 80th column with a:char.
" The argument must be a character.
function! s:InsertComment80(char)

  " Save virtualedit setting.
  let l:vesave = &virtualedit

  " Required.
  set virtualedit=onemore

  let l:num = 81 - s:GetCurColumn()
  let l:cmd = "normal " . l:num . "R" . a:char
  execute l:cmd

  " Load virtualedit setting.
  let &virtualedit = l:vesave

endfunction

"===============================================================================
"===============================================================================
" Command line window.

" Height of cmdwin.
set cmdwinheight=5

" Open cmdwin every time.
nnoremap <SID>(ExCmdwinEnter) q:
xnoremap <SID>(ExCmdwinEnter) q:
nmap : <SID>(ExCmdwinEnter)
xmap : <SID>(ExCmdwinEnter)

augroup MyCmdwin
  autocmd!
  autocmd CmdwinEnter * call s:InitCmdwin()
augroup END

" Called after entering cmdwin.
function! s:InitCmdwin()

  " Quit cmdwin.
  nnoremap <buffer> q :quit<CR>
  nnoremap <buffer> <ESC> :quit<CR>
  inoremap <buffer><expr> <BS> <SID>GetCurColumn()==1 ?
  \ "\<ESC>:quit<CR>" : "\<BS>"

  " Move upward or downward.
  inoremap <buffer> <C-j> <C-g><C-j><ESC>
  inoremap <buffer> <C-k> <C-g><C-k><ESC>
  nnoremap <buffer> <C-j> j
  nnoremap <buffer> <C-k> k

  " Start with insert mode.
  startinsert!

endfunction

"===============================================================================
"===============================================================================
" Generic functions.

" Get cursor column number.
function! s:GetCurColumn()
  let l:CurPos = getcurpos()
  return l:CurPos[2]
endfunction

"===============================================================================
"===============================================================================
" Colorscheme.

"colorscheme myscheme
colorscheme myiceberg

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
nnoremap <ESC><ESC> :nohlsearch<CR>
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
  autocmd QuickFixCmdPost make,*grep* copen
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

" Toggle boolean value.
nnoremap <expr> <C-n> <SID>ToggleBoolean(expand("<cword>"))

function! s:ToggleBoolean(word)

  if (a:word ==# "TRUE")
    return "ciwFALSE\<ESC>"
  elseif (a:word ==# "FALSE")
    return "ciwTRUE\<ESC>"
  elseif (a:word ==# "true")
    return "ciwfalse\<ESC>"
  elseif (a:word ==# "false")
    return "ciwtrue\<ESC>"
  else
    return ""
  endif

endfunction

" Helpgrep to search only for indexes.
command! -nargs=+ HG call s:MyHelpGrep(<q-args>)

function! s:MyHelpGrep(args)
  let str = substitute(a:args,'\s\+','\\S\*','g')
  let str = '\c\*\S*' . str . '\S*\*'
  execute 'helpgrep' str
endfunction

"===============================================================================
"===============================================================================
" Insert mode mappings.

" Prevent inserting Ctrl-F.
inoremap <C-f> <Nop>

" Insert '%'.
inoremap <silent><expr> <C-p> pumvisible() ? "\<C-p>" : "%"

" Insert boolean value.
inoremap <C-f>i .TRUE.
inoremap <C-f>o .FALSE.

" Insert linebreak and continue sentence.
inoremap <silent><expr> <C-f><C-n> <SID>SpecialLineBreak()

" Insert linebreak keeping indent and Fortran comment.
inoremap <silent><expr> <C-f><CR> <SID>SmartLineBreak()

" Insert character toward the 80th column.
inoremap <silent> <C-f>m <C-\><C-o>:call <SID>InsertComment80("-")<CR><Right>
inoremap <silent> <C-f>e <C-\><C-o>:call <SID>InsertComment80("=")<CR><Right>
inoremap <silent> <C-f>x <C-\><C-o>:call <SID>InsertComment80("!")<CR><Right>

" Return string to be typed for linebreak.
function! s:SpecialLineBreak()

  if (&filetype ==# "fortran")
    return "\<Space>&\<CR>&\<Space>"
  elseif (&filetype ==# "vim")
    return "\<CR>\\\<Space>"
  else
    return "\<CR>"
  endif

endfunction

" Return command for linebreak keeping indent and given pattern.
function! s:SmartLineBreak()

  let l:CurLine = s:GetCurLine()
  if (&filetype ==# "fortran")
    let l:string = matchstr(getline(l:CurLine),'^ *!* *')
    return "\<CR>0\<C-d>" . l:string
  elseif (&filetype ==# "vim")
    let l:string = matchstr(getline(l:CurLine),'^ *"* *')
    return "\<CR>0\<C-d>" . l:string
  else
    return "\<CR>"
  endif

endfunction

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
  nnoremap <buffer><nowait> <ESC> :quit<CR>
  inoremap <buffer><expr> <BS> <SID>GetCurColumn()==1 ?
  \ "\<ESC>:quit<CR>" : "\<BS>"
  inoremap <buffer><expr> <C-h> <SID>GetCurColumn()==1 ?
  \ "\<ESC>:quit<CR>" : "\<C-h>"

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

" Get cursor line number.
function! s:GetCurLine()
  let l:CurPos = getcurpos()
  return l:CurPos[1]
endfunction

"===============================================================================
"===============================================================================
" Colorscheme.

"colorscheme myscheme
colorscheme myiceberg

" Highlight over 80th column.
hi! OverLength cterm=reverse ctermfg=NONE ctermbg=NONE

" Automatic matching.
augroup MyAutoMatch
  autocmd!
  autocmd VimEnter,WinEnter * call clearmatches()
  autocmd VimEnter,WinEnter *
  \ if (&modifiable) | call matchadd("OverLength",'.\%>81v') | endif
augroup END

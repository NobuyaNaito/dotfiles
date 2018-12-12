"===============================================================================
" STARTUP
"===============================================================================

" Be IMproved.
if !&compatible
  set nocompatible
endif

" Autocmd group. Used by plugins too.
augroup vimrc
  autocmd!
augroup END



"===============================================================================
" PLUGIN
"===============================================================================

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
" SETTING
"===============================================================================

filetype on
filetype plugin on
filetype indent off

" Runtimepath.
let s:dotfiles_runtime = $HOME . "/dotfiles/vimruntime"
let &runtimepath = s:dotfiles_runtime .",". &runtimepath
let &runtimepath = &runtimepath .",". s:dotfiles_runtime . "/after"

" Search.
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

" other setting
set scrolloff=4
set whichwrap=b,s,<,>,[,]
"set formatoptions-=or   => &runtimepath /after/plugin/common_settings.vim
set formatoptions=q
set backspace=indent,eol,start

" Open quickfix window automatically.
autocmd vimrc QuickFixCmdPost make,*grep* copen

" Open quickfix & help window vertically, in the right and equal widths.
autocmd vimrc FileType qf,help wincmd L

" Uppercase/lowercase adjustment is enabled in ins completion.
" 'set ignorecase' is required.
set infercase

" Options for ins completion.
set completeopt=menu,menuone,preview

autocmd vimrc FileType *
\ if &l:omnifunc == "" | setlocal omnifunc=syntaxcomplete#Complete | endif



"===============================================================================
" MAPPING
"===============================================================================

"-------------------------------------------------------------------------------
" NORMAL MODE

noremap <S-h> ^
noremap <S-l> $
nnoremap <C-h> gT
nnoremap <C-l> gt

" search
nnoremap <ESC><ESC> :nohlsearch<CR>

" Stop completion when editing completed text.
inoremap <expr> <BS> pumvisible() ? "\<C-y>\<BS>" : "\<BS>"

" Toggle boolean value.
nnoremap <silent> <C-n> :call <SID>ToggleBoolean()<CR>

"-------------------------------------------------------------------------------
" INSERT MODE

" Instant escape from insert mode.
inoremap <silent> <ESC> <ESC>:<CR>

" Prevent inserting Ctrl-F.
inoremap <C-f> <Nop>

" Insert '%'.
inoremap <silent><expr> <C-p> pumvisible() ? "\<C-p>" : "%"

" Insert boolean value.
inoremap <C-f>i .TRUE.
inoremap <C-f>o .FALSE.

" Insert linebreak and continue sentence.
inoremap <silent> <C-f><C-n> <ESC>:call <SID>SpecialLineBreak()<CR>a

" Insert linebreak keeping indent and Fortran comment.
inoremap <silent> <C-f><CR> <ESC>:call <SID>SmartLineBreak()<CR>a

" Insert character toward the 80th column.
inoremap <silent> <C-f>m <C-\><C-o>:call <SID>InsertComment80("-")<CR><Right>
inoremap <silent> <C-f>e <C-\><C-o>:call <SID>InsertComment80("=")<CR><Right>
inoremap <silent> <C-f>x <C-\><C-o>:call <SID>InsertComment80("!")<CR><Right>

"-------------------------------------------------------------------------------
" VISUAL MODE

" Instant escape from visual mode.
vnoremap <silent> <ESC> <ESC>:<CR>

" Search selected string not as a single word.
vnoremap * "zy:let @/ = @z<CR>n



"===============================================================================
" COMMAND
"===============================================================================

" Helpgrep to search only for indexes.
command! -nargs=+ HG call s:MyHelpGrep(<q-args>)

" Vimgrep in current file.
command! -nargs=1 VIMGREP vimgrep <args> %

"-------------------------------------------------------------------------------
" COMMAND LINE WINDOW

" Height of cmdwin.
set cmdwinheight=5

" Open cmdwin every time.
nnoremap <SID>(ExCmdwinEnter) q:
xnoremap <SID>(ExCmdwinEnter) q:
nmap : <SID>(ExCmdwinEnter)
xmap : <SID>(ExCmdwinEnter)

autocmd vimrc CmdwinEnter * call s:InitCmdwin()

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
" FUNCTION
"===============================================================================

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

"-------------------------------------------------------------------------------
" Toggle boolean value under the cursor.

function! s:ToggleBoolean()

  " Get word under the cursor.
  let l:word = expand("<cword>")

  " When boolean value, toggle it.
  if (l:word ==# "TRUE")
    execute "normal! ciwFALSE\<ESC>"
  elseif (l:word ==# "FALSE")
    execute "normal! ciwTRUE\<ESC>"
  elseif (l:word ==# "true")
    execute "normal! ciwfalse\<ESC>"
  elseif (l:word ==# "false")
    execute "normal! ciwtrue\<ESC>"
  endif

endfunction

"-------------------------------------------------------------------------------
" Do linebreak and continue the line.
" * Indent in the new line might be deleted.
" * Not start in proper position when not called at end of line.

function! s:SpecialLineBreak()

  if (&filetype ==# "fortran")
    let l:text = "\<Space>&\<CR>&\<Space>"
  elseif (&filetype ==# "vim")
    let l:text = "\<CR>\\\<Space>"
  else
    let l:text = "\<CR>"
  endif

  execute "normal! a" . l:text . "\<ESC>"

endfunction

"-------------------------------------------------------------------------------
" Do linebreak with keeping comment and indent.
" * Listchars might blink when called.
" * Trailing spaces are not trimmed.

function! s:SmartLineBreak()

  let l:curline = s:GetCurLine()

  " String to be inserted.
  if (&filetype ==# "fortran")
    let l:string = matchstr(getline(l:curline),'^ *!* *')
  elseif (&filetype ==# "vim")
    let l:string = matchstr(getline(l:curline),'^ *"* *')
  else
    let l:string = matchstr(getline(l:curline),'^ *#* *')
  endif

  execute "normal! a\<CR>0\<C-d>" . l:string . "\<ESC>"

endfunction

"-------------------------------------------------------------------------------
" Fill from cursor position to 80th column with given character.
" The argument must be one character.

function! s:InsertComment80(char)

  " Save virtualedit setting.
  let l:vesave = &virtualedit

  " Required.
  set virtualedit=onemore

  " The number of inserted characters.
  let l:num = 81 - s:GetCurColumn()

  execute "normal! " . l:num . "R" . a:char

  " Load virtualedit setting.
  let &virtualedit = l:vesave

endfunction

"-------------------------------------------------------------------------------
" Modified helpgrep.

function! s:MyHelpGrep(args)
  let str = substitute(a:args,'\s\+','\\S\*','g')
  let str = '\c\*\S*' . str . '\S*\*'
  execute 'helpgrep' str
endfunction



"===============================================================================
" APPEARANCE
"===============================================================================

syntax enable

set number
set relativenumber
set cursorline

" statusline
set laststatus=2
set ruler

" tabline
set showtabline=2

" Show invisible characters.
set nolist
autocmd vimrc BufWinEnter,WinEnter *
\ if (&modifiable && !&readonly) | set list | else | set nolist | endif

" List of shown invisible characters.
set listchars=tab:>.,trail:.
autocmd vimrc InsertEnter * set listchars=tab:>.
autocmd vimrc InsertLeave * set listchars=tab:>.,trail:.



"===============================================================================
" HIGHLIGHT
"===============================================================================

" Colorscheme.
"colorscheme myscheme
colorscheme myiceberg

" Highlight over 80th column.
hi! OverLength cterm=reverse ctermfg=NONE ctermbg=NONE

" Automatic matching.
autocmd vimrc BufWinEnter,WinEnter * call clearmatches()
autocmd vimrc BufWinEnter,WinEnter * if (&modifiable && !&readonly)
\ | call matchadd("OverLength",'.\%>81v') | endif

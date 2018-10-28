if !&compatible
  set nocompatible
endif

" command
"command! -nargs=? -complete=file TE tabedit <args>

" mapping
noremap <S-h> ^
noremap <S-l> $
nnoremap <C-h> gT
nnoremap <C-l> gt
"nnoremap :te :tabedit
vnoremap * "zy:let @/ = @z<CR>n
" instant escape from insert and visual mode
inoremap <silent> <ESC> <ESC>:<RETURN>
vnoremap <silent> <ESC> <ESC>:<RETURN>

" search
nnoremap <ESC><ESC> :set nohlsearch<RETURN>
nnoremap / :set hlsearch<RETURN>/
nnoremap ? :set hlsearch<RETURN>?
nnoremap * :set hlsearch<RETURN>*
nnoremap # :set hlsearch<RETURN>#
nnoremap g* :set hlsearch<RETURN>g*
nnoremap g# :set hlsearch<RETURN>g#
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
"set formatoptions-=or   ~/.vim/after/plugin/common_settings.vim に記述
set formatoptions=q
set backspace=indent,eol,start

augroup QuickFixCmd
  autocmd!
  autocmd QuickFixCmdPost make,*grep* cwindow
augroup END

" sp, vsp 等を実行した際、編集中のファイルのみカーソルラインのハイライトが
" 有効になるように設定中。今はまだ正しく動かない。
"autocmd ColorScheme * highlight CursorLineNr ctermfg=242 ctermbg=none
"autocmd ColorScheme * highlight CursorLine cterm=none ctermfg=none ctermbg=none
"augroup cursorline_highlight_on
"  autocmd!
"  autocmd VimEnter,BufEnter * highlight CursorLineNr ctermfg=231 ctermbg=236
"  autocmd VimEnter,BufEnter * highlight CursorLine cterm=none ctermfg=none ctermbg=236
"augroup END
"augroup cursorline_highlight_off
"  autocmd!
"  autocmd BufLeave * highlight CursorLineNr ctermfg=242 ctermbg=none
"  autocmd BufLeave * highlight CursorLine cterm=none ctermfg=none ctermbg=none
"augroup END
"autocmd VimEnter,BufEnter * doautocmd cursorline_highlight_on VimEnter,BufEnter
"autocmd BufLeave * doautocmd cursorline_highlight_off BufLeave

filetype on
filetype plugin on
filetype indent off

"===============================================================================
"===============================================================================
" plugins

let s:plugin_dir = $HOME . "/.vim/bundles"
let s:dein_dir = s:plugin_dir . "/repos/github.com/Shougo/dein.vim"

" When dein is installed.
if isdirectory(s:dein_dir)

  let &runtimepath = s:dein_dir .",". &runtimepath

  if dein#load_state(s:plugin_dir)
    call dein#begin(s:plugin_dir)
    call dein#load_toml(s:plugin_dir . "/dein.toml")
    call dein#end()
    call dein#save_state()
  endif

  " If you want to install not installed plugins on startup.
  if dein#check_install()
    call dein#install()
  endif

  if dein#is_sourced("neosnippet.vim")
    command! Snip NeoSnippetEdit

    " # Plugin key-mappings.
    " # Note: It must be "imap" and "smap".  It uses <Plug> mappings.
    imap <C-j> <Plug>(neosnippet_expand_or_jump)
    smap <C-j> <Plug>(neosnippet_expand_or_jump)
    xmap <C-j> <Plug>(neosnippet_expand_target)

    " # SuperTab like snippets behavior.
    " # Note: It must be "imap" and "smap".  It uses <Plug> mappings.
    "# imap <expr><TAB>
    " # \ pumvisible() ? "\<C-n>" :
    " # \ neosnippet#expandable_or_jumpable() ?
    " # \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
    smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
    \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
  endif

endif

"===============================================================================
"===============================================================================
" colorscheme

syntax on
"colorscheme myscheme
colorscheme myiceberg

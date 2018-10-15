" plugins
if &compatible
  set nocompatible
endif
  
"set runtimepath+=/home/naito/.vim/bundles/repos/github.com/Shougo/dein.vim
"
"if dein#load_state('/home/naito/.vim/bundles')
"  call dein#begin('/home/naito/.vim/bundles')
"
"  " Let dein manage dein.
"  call dein#add('/home/naito/.vim/bundles/repos/github.com/Shougo/dein.vim')
"
"  " Add or remove your plugins here:
"  call dein#add('Shougo/neosnippet.vim')
"  call dein#add('Shougo/neosnippet-snippets')
"  call dein#add('tpope/vim-fugitive')
"
"  " You can specify revision/branch/tag.
"  "call dein#add('Shougo/deol.nvim', { 'rev': 'a1b5108fd' })
"
"  call dein#end()
"  call dein#save_state()
"endif

filetype on
filetype plugin on
filetype indent off

colorscheme myscheme
syntax enable
let g:syntax_cmd="skip" "Avoid reading in syncolor.vim.

" If you want to install not installed plugins on startup.
"if dein#check_install()
"  call dein#install()
"endif

"===============================================================================

" command
command! -nargs=? -complete=file TE tabedit <args>

" mapping
noremap <S-h> ^
noremap <S-l> $
nnoremap <C-h> gT
nnoremap <C-l> gt
"nnoremap :te :tabedit
vnoremap * "zy:let @/ = @z<CR>n

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
execute "set colorcolumn=" . join(range(81, 999), ',')

" other setting
set scrolloff=4
set whichwrap=b,s,<,>,[,]
set number
set cursorline
"set formatoptions-=or   ~/.vim/after/plugin/common_settings.vim に記述
set formatoptions=q
set backspace=indent,eol,start
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

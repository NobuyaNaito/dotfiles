" plugins
"if &compatible
"  set nocompatible
"endif
"  
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
"
filetype on
filetype plugin on
filetype indent off
syntax enable

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
"  autocmd VimEnter,BufEnter * highlight CursorLineNr ctermfg=15 ctermbg=236
"  autocmd VimEnter,BufEnter * highlight CursorLine cterm=none ctermfg=none ctermbg=236
"augroup END
"augroup cursorline_highlight_off
"  autocmd!
"  autocmd BufLeave * highlight CursorLineNr ctermfg=242 ctermbg=none
"  autocmd BufLeave * highlight CursorLine cterm=none ctermfg=none ctermbg=none
"augroup END
"autocmd VimEnter,BufEnter * doautocmd cursorline_highlight_on VimEnter,BufEnter
"autocmd BufLeave * doautocmd cursorline_highlight_off BufLeave

"===============================================================================

" colorschemes

" basic
autocmd ColorScheme * highlight Comment ctermfg=68 ctermbg=none
autocmd ColorScheme * highlight Constant ctermfg=197 ctermbg=none
autocmd ColorScheme * highlight Identifier ctermfg=81 ctermbg=none
autocmd ColorScheme * highlight Statement ctermfg=227 ctermbg=none
autocmd ColorScheme * highlight PreProc ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Type ctermfg=112 ctermbg=none
autocmd ColorScheme * highlight Special ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Underlined cterm=underline ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Error cterm=none ctermfg=15 ctermbg=9
autocmd ColorScheme * highlight Todo cterm=none ctermfg=0 ctermbg=11
autocmd ColorScheme * highlight ErrorMsg cterm=none ctermfg=15 ctermbg=9
autocmd ColorScheme * highlight NonText ctermfg=242 ctermbg=none

" search
autocmd ColorScheme * highlight Search cterm=none ctermfg=0 ctermbg=11
autocmd ColorScheme * highlight IncSearch cterm=none ctermfg=0 ctermbg=15

" statusline
autocmd ColorScheme * highlight StatusLine cterm=bold ctermfg=15 ctermbg=60
autocmd ColorScheme * highlight StatusLineNC cterm=none ctermfg=189 ctermbg=60
autocmd ColorScheme * highlight VertSplit cterm=bold ctermfg=15 ctermbg=0

" tabline
autocmd ColorScheme * highlight TabLine cterm=none ctermfg=189 ctermbg=60
autocmd ColorScheme * highlight TabLineSel cterm=bold ctermfg=15 ctermbg=0
autocmd ColorScheme * highlight TabLineFill cterm=none ctermfg=189 ctermbg=60

" linenumber and cursorline
autocmd ColorScheme * highlight LineNr ctermfg=242 ctermbg=none
autocmd ColorScheme * highlight CursorLineNr ctermfg=15 ctermbg=234
autocmd ColorScheme * highlight CursorLine cterm=none ctermfg=none ctermbg=234

" column
autocmd ColorScheme * highlight ColorColumn cterm=none ctermfg=none ctermbg=234

" git diff
autocmd ColorScheme * highlight DiffAdd cterm=bold ctermfg=16 ctermbg=203
autocmd ColorScheme * highlight DiffChange cterm=none ctermfg=16 ctermbg=210
autocmd ColorScheme * highlight DiffDelete cterm=none ctermfg=16 ctermbg=210
autocmd ColorScheme * highlight DiffText cterm=bold ctermfg=16 ctermbg=203
autocmd ColorScheme * highlight Folded cterm=none ctermfg=16 ctermbg=189
autocmd ColorScheme * highlight FoldColumn cterm=bold ctermfg=15 ctermbg=16

colorscheme default

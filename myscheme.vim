" Originally defined vim colorscheme.
" Only supports CUI version and dark background.
" Put this file in <~/.vim/colors/>.

let g:colors_name = expand('<sfile>:t:r')

set background=dark

" Remove all existing highlighting and set the defaults.
highlight clear

" Load the syntax highlighting defaults, if it's enabled.
if exists("syntax_on")
  syntax reset
endif

"===============================================================================

" general
autocmd ColorScheme * highlight Normal cterm=none ctermfg=231 ctermbg=none
autocmd ColorScheme * highlight Comment cterm=none ctermfg=68 ctermbg=none
autocmd ColorScheme * highlight Constant cterm=none ctermfg=197 ctermbg=none
autocmd ColorScheme * highlight Identifier cterm=none ctermfg=81 ctermbg=none
autocmd ColorScheme * highlight Statement cterm=none ctermfg=227 ctermbg=none
autocmd ColorScheme * highlight PreProc cterm=none ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Type cterm=none ctermfg=112 ctermbg=none
autocmd ColorScheme * highlight Special cterm=none ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Underlined cterm=underline ctermfg=206 ctermbg=none
autocmd ColorScheme * highlight Error cterm=none ctermfg=231 ctermbg=196
autocmd ColorScheme * highlight Todo cterm=none ctermfg=16 ctermbg=226
autocmd ColorScheme * highlight ErrorMsg cterm=none ctermfg=231 ctermbg=196
autocmd ColorScheme * highlight NonText cterm=none ctermfg=242 ctermbg=none

" full width space
autocmd ColorScheme * highlight FullWidthSpace cterm=none ctermfg=none ctermbg=231
autocmd VimEnter * match FullWidthSpace /　/

" search
autocmd ColorScheme * highlight Search cterm=none ctermfg=16 ctermbg=226
autocmd ColorScheme * highlight IncSearch cterm=none ctermfg=16 ctermbg=231

" statusline
autocmd ColorScheme * highlight StatusLine cterm=bold ctermfg=231 ctermbg=60
autocmd ColorScheme * highlight StatusLineNC cterm=none ctermfg=189 ctermbg=60
autocmd ColorScheme * highlight VertSplit cterm=bold ctermfg=231 ctermbg=16

" tabline
autocmd ColorScheme * highlight TabLine cterm=none ctermfg=189 ctermbg=60
autocmd ColorScheme * highlight TabLineSel cterm=bold ctermfg=231 ctermbg=16
autocmd ColorScheme * highlight TabLineFill cterm=none ctermfg=189 ctermbg=60

" linenumber and cursorline
autocmd ColorScheme * highlight LineNr cterm=none ctermfg=242 ctermbg=none
autocmd ColorScheme * highlight CursorLineNr cterm=none ctermfg=231 ctermbg=234
autocmd ColorScheme * highlight CursorLine cterm=none ctermfg=none ctermbg=234

" column
autocmd ColorScheme * highlight ColorColumn cterm=none ctermfg=none ctermbg=234

" git diff
autocmd ColorScheme * highlight DiffAdd cterm=bold ctermfg=16 ctermbg=203
autocmd ColorScheme * highlight DiffChange cterm=none ctermfg=16 ctermbg=210
autocmd ColorScheme * highlight DiffDelete cterm=none ctermfg=16 ctermbg=210
autocmd ColorScheme * highlight DiffText cterm=bold ctermfg=16 ctermbg=203
autocmd ColorScheme * highlight Folded cterm=none ctermfg=16 ctermbg=189
autocmd ColorScheme * highlight FoldColumn cterm=bold ctermfg=231 ctermbg=16
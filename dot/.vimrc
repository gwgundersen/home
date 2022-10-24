" gwg vim config

" ------------------------------------------------------------------------------

call plug#begin()

" Fuzzy find/search.
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Real-time linting.
Plug 'dense-analysis/ale'

" File tree view.
Plug 'lambdalisue/fern.vim'

" Color scheme based on Sublime Text.
Plug 'crusoexia/vim-monokai'

call plug#end()

" ------------------------------------------------------------------------------

set paste            " Allow CTRL+V pasting from clipboard.
set number           " Line numbers.
set colorcolumn=80   " Vertical line at char 80.
set tabstop=4        " Show existing tab with 4 spaces width.
set shiftwidth=4     " When indenting with '>', use 4 spaces width.
set expandtab        " On pressing tab, insert 4 spaces.

syntax on
colorscheme monokai  " Enable color scheme.

nnoremap <C-t> <C-w>

" Tree view.
let g:netrw_banner = 0         " Remove banner.
let g:netrw_liststyle = 3      " Set tree style.
let g:netrw_browse_split = 4   " 
let g:netrw_altv = 1           " Show tree view in list on LHS.
let g:netrw_winsize = 15       " Set tree view width.
augroup ProjectDrawer
  autocmd!
  autocmd VimEnter * :Vexplore
augroup END

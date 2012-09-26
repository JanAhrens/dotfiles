set nocompatible

" Pathogen
call pathogen#infect()
syntax on
filetype plugin indent on

set tabstop=2 shiftwidth=2 expandtab
set textwidth=120
set nowrap
set autoindent
set hlsearch
set ignorecase smartcase
set number

" Automatically insert the current comment leader after
" hitting 'o' or 'O' in Normal mode.
set formatoptions+=r

" Automatically insert the current comment leader after
" hitting <Enter> in Insert mode
set formatoptions+=o

" When formatting text, use the indent of the second line of
" a paragraph for the rest of the paragraph, instead of the
" indent of the first line
set formatoptions+=2

" All the windows are automatically made the same size after
" splitting or closing a window
set equalalways

" Splitting a window will put the new window right of the
" current one
set splitright

" enable the use of the mouse in terminals
set mouse=a ttymouse=xterm2

" Indicates a fast terminal connection. More characters will be sent to the
" screen for redrawing, instead of using insert/delete line commands.
set ttyfast

" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#using-the-leader
let mapleader=","

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

autocmd BufRead,BufNewFile Makefile setlocal noexpandtab
autocmd FileType mail setlocal textwidth=72
autocmd BufRead .git/COMMIT_EDITMSG setlocal textwidth=72

" show the matched parenthesis for 0.3 seconds
set showmatch matchtime=3
highlight MatchParen ctermbg=white

" ack
let g:ackprg="ack -H --nocolor --nogroup --column"

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*.so

noremap <cr> :nohlsearch<cr>
noremap <leader>e :NERDTreeToggle<cr>
noremap <leader>f :NERDTreeFind<cr>

" fast window switching
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

"display tabs and trailing spaces
set list
set listchars=trail:⋅,nbsp:⋅,tab:>-

set clipboard+=unnamed

" always display the tabline
set showtabline=2

" Switch between the last two files
nnoremap <leader><leader> <c-^>

" Shortcut for expanding to the directory of the currently displayed file
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" Shortcut for expanding to full filename of the currently displayed file
cnoremap $$ <C-R>=expand('%')<cr>

highlight Pmenu ctermbg=238 gui=bold

source ~/.vimrc.local

set nocompatible

" Pathogen
call pathogen#infect()
syntax on
filetype plugin indent on

colorscheme pablo

set tabstop=2 shiftwidth=2 expandtab
set textwidth=120
set nowrap
set autoindent
set hlsearch
set ignorecase smartcase
set number

" don't give the intro message when starting Vim :intro.
set shortmess+=I

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

" Characters to fill the statusline and vertical seperators
set fillchars=vert:\ ,fold:\ 

" enable the use of the mouse in terminals
set mouse=a ttymouse=xterm2

" Indicates a fast terminal connection. More characters will be sent to the
" screen for redrawing, instead of using insert/delete line commands.
set ttyfast

autocmd BufRead,BufNewFile Makefile setlocal noexpandtab

autocmd FileType mail setlocal textwidth=72
autocmd BufRead .git/COMMIT_EDITMSG setlocal textwidth=72

" show the matched parenthesis for 0.3 seconds
set showmatch matchtime=3
highlight MatchParen ctermbg=white

" ack
let g:ackprg="ack -H --nocolor --nogroup --column"

runtime macros/matchit.vim

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*.so

" CtrlP
let g:ctrlp_working_path_mode = 2
let g:ctrlp_use_caching = 1
let g:ctrlp_clear_cache_on_exit = 0

" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#using-the-leader
let mapleader=","

inoremap jj <esc>
" 'promote' the new <esc> key
"inoremap <esc> <nop>

noremap <leader>s :set spell!<cr>
noremap <leader>n :nohlsearch<cr>
noremap <leader>p :CtrlP<cr>
noremap <leader>e :NERDTreeToggle<cr>
noremap <leader>f :NERDTreeFind<cr>

noremap <leader>gb :Gblame<cr>
noremap <leader>gl :Glog<cr>

" fast window switching
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

noremap g0 :tabfirst<cr>
noremap g$ :tablast<cr>

" define the key that toogle the paste insert mode
set pastetoggle=<Leader>p

"display tabs and trailing spaces
set list
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅

set ruler
set clipboard+=unnamed
source ~/.vimrc.local

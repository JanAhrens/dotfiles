set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
Bundle 'gmarik/vundle'

Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'mileszs/ack.vim'
Bundle 'wikitopian/hardmode'
Bundle 'ervandew/supertab'
Bundle 'scrooloose/nerdtree'
Bundle 'wincent/Command-T'
Bundle 'edsono/vim-matchit'
Bundle 'janx/vim-rubytest'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-powerline'

Bundle 'kana/vim-textobj-user'
Bundle 'nelstrom/vim-textobj-rubyblock'

filetype plugin indent on
syntax on

colorscheme darkblue

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

" source https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
noremap <cr> :nohlsearch<cr>
autocmd CmdwinEnter * :unmap <cr>

noremap <leader>e :NERDTreeToggle<cr>
noremap <leader>f :NERDTreeFind<cr>

" fast window switching
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

nnoremap <silent> <Leader>n :CommandT<CR>
nnoremap <silent> <Leader>b :CommandTBuffer<CR>

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

imap jj <esc>

nnoremap <silent> <Leader>n :CommandT<CR>
nnoremap <silent> <Leader>b :CommandTBuffer<CR>

source ~/.vimrc.local

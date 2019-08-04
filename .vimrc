syntax on

" // syntax -----------------------------------------------------------
colorscheme default

" // options ----------------------------------------------------------
set tabstop=2 shiftwidth=2 expandtab
set textwidth=120
set nowrap
set autoindent
set hlsearch
set smartcase

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

" show the matched parenthesis for 0.3 seconds
set showmatch matchtime=3

set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*.so

" always display the tabline
set showtabline=2

set iskeyword+=-

" // pattern ----------------------------------------------------------
set ignorecase

" // various ----------------------------------------------------------
set number

"display tabs and trailing spaces
set list

set background=dark

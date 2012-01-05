" NOTE: for GUI settings see .gvimrc

" Use the IMprovements
set nocompatible

call pathogen#infect()
syntax on 
filetype plugin indent on
:Helptags

" don't give the intro message when starting Vim :intro
set shortmess+=I

" Number of spaces that a <Tab> in the file counts for
set tabstop=2

" Number of spaces to use for each step of (auto)indent
set shiftwidth=2

" Lines will not  wrap and only part of long lines will be
" displayed. When the cursor is moved to a part that is not
" show, the screen will scroll horizontally
set nowrap

" Copy indent from current line wehn starting a new line
set autoindent

" Write the contents of the file, if it has been modified
set autowrite

" When there is a previous search pattern, highlight all its
" matches
set hlsearch

" Ignore the case of normal letters in patterns
set ignorecase

" Override the 'ignorecase' option if the search pattern
" contains upper case characters
set smartcase

" Show the line and column number of the cursor position
set ruler

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

" Wrap when using <Left> and <Right> in Normal, Visual,
" Insert and Replace 
set whichwrap=b,s,<,>,[,]

" enable the use of the mouse in terminals
set mouse=a

" define the key that toogle the paste insert mode
set pastetoggle=<F2>

" In Insert mode: Use the appropriate number of spaces to
" insert a <Tab>
set expandtab
autocmd BufRead,BufNewFile Makefile setlocal noexpandtab

set textwidth=120
autocmd FileType mail setlocal textwidth=72
autocmd BufRead .git/COMMIT_EDITMSG setlocal textwidth=72

" show the matched parenthesis for 0.3 seconds
set showmatch matchtime=3
highlight MatchParen ctermbg=white

" Folding
set foldcolumn=0
set foldmethod=manual
hi FoldColumn guifg=#333 guibg=#FFF

set number
highlight LineNr ctermfg=grey guifg=grey

set colorcolumn=+0
highlight ColorColumn ctermbg=lightgrey guibg=lightgrey
autocmd FileType nerdtree set colorcolumn=
autocmd FileType qf set colorcolumn=
autocmd FileType help set colorcolumn=

let s:color_column_old = +0
function! s:ToggleColorColumn()
    if s:color_column_old == ''
        let s:color_column_old = &colorcolumn
        let &colorcolumn = +0 ""
    else
        let &colorcolumn=s:color_column_old ""
        let s:color_column_old = +0
    endif
endfunction

map <F3> :call <SID>ToggleColorColumn()<cr>

" ,,Learning, the hard way''
" http://cloudhead.io/2010/04/24/staying-the-hell-out-of-insert-mode/
inoremap <Left>  <NOP>
inoremap <Right> <NOP>
inoremap <Up>    <NOP>
inoremap <Down>  <NOP>

" Move a line up with Alt-Up and move it down with Alt-Down
nnoremap <A-Up>        :m-2<CR>  ==
nnoremap <A-Down>      :m+ <CR>  ==
vnoremap <A-Up>        :m  -2<CR>gv= gv
vnoremap <A-Down>      :m'>+ <CR>gv= gv

" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#using-the-leader
let mapleader=","

map <F12> :set spell!<cr>

" <Ctrl-l> redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" ----------------------------------------------------------
" Plugins
" -------

" NerdTree
let NERDTreeIgnore=['\.class$', '\~$', '\.bak$']
nmap <silent> <c-t> :NERDTreeToggle<cr>
inoremap <c-t> <esc>:NERDTreeClose<cr>a

" Gundo
nnoremap <F5> :GundoToggle<CR>

" minibufexpl
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" lesscss
autocmd BufNewFile,BufRead *.less set filetype=less
      
" tmru
noremap <c-r> :TRecentlyUsedFiles<cr> 

" ack
let g:ackprg="ack-grep -H --nocolor --nogroup --column"
map <leader>a :NERDTreeClose<cr>:Ack<space>

" fugetive
set statusline="%{fugitive#statusline()}"
" ----------------------------------------------------------

source ~/.vimrc.local

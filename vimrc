" NOTE: for gvim settings see ~/.gvimrc

syntax on 

set shortmess+=I " don't give the intro message when startgin Vim :intro
set tabstop=2 shiftwidth=2 expandtab
set textwidth=120
set nowrap
set autoindent
set autowrite
set hlsearch ignorecase
set ruler
set formatoptions=2croql
set equalalways splitright
set fillchars=vert:\ ,fold:\ 
set whichwrap=<,>,h,l,[,]

set showmatch matchtime=3 " show the matched parenthesis for 0.3 seconds
highlight MatchParen ctermbg=white

" Folding
set foldcolumn=1
set foldmethod=manual
hi FoldColumn guifg=#333 guibg=#FFF

set number
highlight LineNr ctermfg=grey guifg=grey

set colorcolumn=+0
highlight ColorColumn ctermbg=lightgrey guibg=lightgrey

" automatically reload the vimrc if its modified
autocmd BufWritePost .vimrc source %

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

map <F12> :set spell!<cr>

" ----------------------------------------------------------
" plugins

" Pathogen
call pathogen#runtime_append_all_bundles()

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

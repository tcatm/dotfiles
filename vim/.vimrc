" Necesary  for lots of cool vim things
set nocompatible

filetype plugin on
filetype indent on

set incsearch       " incremental searching
set hlsearch        " highlight matches
set ignorecase      " searches are case insensitive...
set smartcase       " ... unless they contain at least one capital letter

set softtabstop=2
set shiftwidth=2
set tabstop=2
set expandtab
set nu

set smarttab
set autoindent

syntax on

au BufRead,BufNewFile *.xp set filetype=c
au BufRead,BufNewFile *.xp set makeprg=elaps\ %
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") |  exe "normal g'\"" | endif

set encoding=utf-8

set nofoldenable
set nofsync
set hidden

map <F2> :NERDTreeToggle<CR>
map <F5> :w<CR>:!make<CR>
map <F7> :make<CR>

" Remove any trailing whitespace that is in the file
autocmd BufRead,BufWrite *.py if ! &bin | silent! %s/\s\+$//ge | endif
autocmd BufRead,BufWrite *.css if ! &bin | silent! %s/\s\+$//ge | endif
autocmd BufRead,BufWrite *.less if ! &bin | silent! %s/\s\+$//ge | endif
autocmd BufRead,BufWrite *.c if ! &bin | silent! %s/\s\+$//ge | endif
autocmd BufRead,BufWrite *.html if ! &bin | silent! %s/\s\+$//ge | endif

autocmd BufRead,BufWrite *.mustache set syntax=html

" This shows what you are typing as a command.  I love this!
set showcmd

set smartcase

if has("gui_running")
   " Remove Toolbar
   set guioptions-=T
   set guifont=Input\ Mono\ Compressed\ 11
endif

" highlight current line
"set cul
"hi CursorLine term=none cterm=none ctermbg=4

"Status line gnarliness
set laststatus=2
set statusline=%F%m%r%h%w\ (%{&ff}){%Y}\ [%l,%v][%p%%]

command WQ wq
command Wq wq
command W w
command Q q

map <C-j> :bprev<CR>
map <C-k> :bnext<CR>
set hidden " this will go along

" Set tab behaviour depending on file extensions
" softtab, 2 spaces for haskell
autocmd BufRead,BufNewFile *.hs set softtabstop=2 shiftwidth=2 expandtab
autocmd BufRead,BufNewFile *.html set softtabstop=2 shiftwidth=2 expandtab
autocmd BufRead,BufNewFile *.less set softtabstop=2 shiftwidth=2 expandtab
autocmd BufRead,BufNewFile *.css set softtabstop=2 shiftwidth=2 expandtab
autocmd BufRead,BufNewFile *.rb set softtabstop=2 shiftwidth=2 expandtab

autocmd BufRead,BufNewFile *.txt set textwidth=80
autocmd BufRead,BufNewFile *.md set textwidth=80

autocmd BufRead,BufNewFile *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class

autocmd BufRead,BufNewFile *.txt set tw=79
autocmd BufRead,BufNewFile *.txt set expandtab

autocmd BufRead,BufNewFile *.md set tw=79
autocmd BufRead,BufNewFile *.md set tabstop=4 softtabstop=4 shiftwidth=4
autocmd BufRead,BufNewFile *.md set expandtab

au BufRead *.c call YAIFA(1)
au BufRead *.h call YAIFA(1)
au BufRead *.cpp call YAIFA(1)
au BufRead *.py call YAIFA(1)

"always start on first line on git commit
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

"set colorcolumn=80

set directory=/home/nils/volatile/vimswaps/
set backupdir=/home/nils/volatile/vimswaps/

setlocal omnifunc=syntaxcomplete#Complete

nnoremap <F3> :NumbersToggle<CR> 

set mouse=a
set ttyfast
set ttymouse=xterm2

set nocursorcolumn
set nocursorline
syntax sync minlines=256

set t_Co=256
set background=dark

" Favorite Color Scheme
"colorscheme solarized
"colorscheme desert256
"colorscheme seoul256
colorscheme jellybeans
"colorscheme distinguished
"colorscheme zenburn

set exrc
set secure

set cc=78

set relativenumber

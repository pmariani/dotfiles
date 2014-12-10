set nocompatible
set backspace=indent,eol,start
set autoindent
set ruler
set number
set showcmd
set incsearch
set hlsearch
set list
set listchars=tab:>-,trail:-
set foldcolumn=2
set foldenable
set foldmethod=indent
set background=dark
set hidden
set expandtab
set tabstop=2
set softtabstop=2
set shiftwidth=2
filetype plugin indent on
syntax on
set nowrap
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" echom ">^.^<"
let mapleader = ","
set nocompatible
set showmode
set showcmd
set guioptions -=T
set guifont=consolas:h12
colorscheme desert 
" MY THEMES : desert murphy elflord
syntax on
set number
set relativenumber
set ruler
set encoding=utf-8
set tabstop=4
set shiftwidth=4
set expandtab
set laststatus=2
set autoindent
set foldmethod=manual
set hlsearch
set incsearch
set wildmenu
set showmatch
filetype plugin indent on
autocmd FileType typescript setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 softtabstop=2 expandtab
" Easy upper case whole word
inoremap <c-u> <esc>bveUea
nnoremap <c-u> bveUe

nnoremap <Leader>ev :split $MYVIMRC<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>

function RecordInterestingCommandWithComment()
    let @r=@:
    redir >> C:\Users\pimarian.REDMOND\Desktop\vim-interesting.txt | silent echo @r | redir END
    echon "That last command was interesting, saved it"
endfunction

noremap <Leader>r :call RecordInterestingCommandWithComment()<cr>

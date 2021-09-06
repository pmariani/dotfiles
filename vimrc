filetype off
" set shellslash
" set rtp+=~/vimfiles/bundle/Vundle.vim
" call vundle#begin('~/vimfiles/bundle')
" let Vundle manage Vundle, required
" Plugin 'VundleVim/Vundle.vim'
" Plugin 'preservim/nerdtree'

" All of your Plugins must be added before the following line
"call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line"
echo ">^.^<\nF1 for my commands"
let mapleader = ","
set nocompatible
set showmode
set showcmd
set nowrap
set guioptions -=T
set guioptions -=m
set guifont=consolas:h12
colorscheme murphy
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
set ignorecase
set wildmenu
set showmatch
" set lines=51 columns=122
set nofixendofline
set hidden " be able to navigate away from changed buffers
set textwidth=80
set colorcolumn=80
" winpos 669 18
nohlsearch
set vb t_vb=
set backspace=indent,eol,start
set autochdir
set directory=$HOME/vimfiles/swaps//
set backupdir=$HOME/vimfiles/swaps//
set undodir=$HOME/vimfiles/swaps//
filetype plugin indent on
autocmd FileType typescript setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 softtabstop=2 expandtab
" Easy upper case whole word
inoremap <c-u> <esc>bveUea
" nnoremap <c-u> bveUe

nnoremap <Leader>ev :split $MYVIMRC<cr>
nnoremap <Leader>sv :source $MYVIMRC<cr>
nnoremap <Leader>f zf%
nnoremap <Leader>F zf}
nnoremap <Leader>w /\s$<cr>
nnoremap <Leader>c :bp\|bd #<cr>
nnoremap <c-tab> :tabnext<cr>
nnoremap <f8> :NERDTree .\<cr>
nnoremap <c-n> :tabnew<cr>

function RecordInterestingCommandWithComment()
    let @r=@:
    redir >> $HOME/vimfiles/interesting-commands.txt | silent echo @r | redir END
    echon "That last command was interesting, saved it"
endfunction

function! DisplayInterestingCommands()
    let @f=$HOME . "/vimfiles/interesting-commands.txt"
    "echo readfile(@f)
    "echo @f
    "execute edit @f
    execute "edit ". fnameescape(@f)
endfunction

noremap <Leader>r :call RecordInterestingCommandWithComment()<cr>
nnoremap <f1> :call DisplayInterestingCommands()<cr>



call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'
Plug 'prabirshrestha/asyncomplete.vim'
Plug 'prabirshrestha/asyncomplete-lsp.vim'

call plug#end()

set list
set listchars=tab:--,trail:.,eol:¬,extends:>,precedes:<

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"


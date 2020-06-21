function! ConfigureView()
    " set shell=/bin/sh
    set noswapfile
    set number
    set signcolumn=yes
    set incsearch
    set hlsearch
    set ignorecase
    set smartcase
    set termencoding=utf8
    set nocompatible
    set ruler
    set showcmd
    set foldenable
    set foldlevel=100
    set foldmethod=indent
    set noerrorbells visualbell t_vb=
    autocmd GUIEnter * set visualbell t_vb=
    set mouse=a
    set mousemodel=popup
    set hidden
    set guioptions-=T
    set ch=1
    set mousehide
    set autoindent
    set nowrap
    set expandtab
    set shiftwidth=4
    set softtabstop=4
    set tabstop=4
    set smarttab
    set laststatus=2
    set smartindent
    set showmatch
    "set lines=30
    "set columns=110
    set iskeyword=@,48-57,_,192-255
    set backspace=indent,eol,start
    set cursorline
    syntax enable
    highlight CursorLine guibg=lightblue ctermbg=lightgray
    highlight CursorLine term=none cterm=none
    set history=200
    set wildmenu
    set list listchars=tab:→\ ,trail:·
    filetype plugin on
    set colorcolumn=80
    set completeopt-=preview
    set cmdheight=1
    let mapleader = "\\"

    set t_Co=256
    set background=light
    colorscheme PaperColor
endfunc

function! InitExternalPlugins()
    if has('win32')
        let g:python3_host_prog = 'C:\Python37\python.exe'
    endif
    " TagBar
    let g:tagbar_left = 1
    let g:tagbar_autofocus = 1

    "" Neomake
    let g:neomake_open_list = 2

    " NERDCommenter
    let g:NERDSpaceDelims = 1

    let g:deoplete#enable_at_startup = 1
    set completeopt=menu,noinsert " auto select first element in the popup list

    autocmd BufWrite *.go,*.cpp,*.hpp,*.c,*.h :Autoformat

    let g:LanguageClient_serverCommands = { 'cpp': ['clangd-10', '-header-insertion=never'],
                \ 'haskell': ['hie-wrapper', '--lsp'],
                \ 'go': ['gopls'] }

    let g:go_def_mode='gopls'
    let g:go_info_mode='gopls'
endfunction

function! BindKeys()
    nnoremap <F5> :call LanguageClient_contextMenu()<CR>
    map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
    map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
    map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
    map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
    map <Leader>lb :call LanguageClient#textDocument_references()<CR>
    map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
    map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

    map <Leader>b <esc>:Buffers<cr>
    map <Leader>p <esc>:Files<cr>
    map <Leader>t <esc>:Tags<cr>
    map <silent> <Leader>s :Rg <C-R><C-W><CR>
    nmap <silent> <Leader>A :FSHere<cr>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NERDTreeToggle<CR>
    nmap <Leader>f :NERDTreeFind<CR>
    nmap <f12> :Autoformat<CR>
    nmap <silent> <Leader>c :Neomake!<cr>
endfunction

function! Spelling()
    autocmd BufRead,BufNewFile *.md setlocal spell
    autocmd BufRead,BufNewFile *.txt setlocal spell
    autocmd FileType gitcommit setlocal spell
    set complete+=kspell
endfunc

call plug#begin()
Plug 'NLKNguyen/papercolor-theme'

Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', {
            \ 'dir': '~/.fzf',
            \ 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'Chiel92/vim-autoformat'
Plug 'neomake/neomake'
Plug 'tpope/vim-fugitive'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins'  }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'ludovicchabant/vim-gutentags'
Plug 'universal-ctags/ctags'
Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh' }

Plug 'hashivim/vim-terraform'
Plug 'chr4/nginx.vim'
Plug 'fatih/vim-go'
Plug 'bfrg/vim-cpp-modern'
Plug 'derekwyatt/vim-fswitch'
Plug 'ekalinin/Dockerfile.vim'
Plug 'tpope/vim-surround'
Plug 'jceb/vim-orgmode'
call plug#end()

call BindKeys()
call ConfigureView()
call InitExternalPlugins()
call Spelling()


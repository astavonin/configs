function! ConfigureView()
    set shell=/bin/sh
    set noswapfile
    set number
    syntax on
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
    set lines=30
    set columns=110
    set iskeyword=@,48-57,_,192-255
    set backspace=indent,eol,start
    set cursorline
    highlight CursorLine guibg=lightblue ctermbg=lightgray
    highlight CursorLine term=none cterm=none
    set history=200
    set wildmenu
    set list listchars=tab:→\ ,trail:·
    filetype plugin on
    set colorcolumn=80
    set completeopt-=preview
    set cmdheight=1
endfunc


function! InitExternalPlugins()
    " TagBar
    let g:tagbar_left = 1
    let g:tagbar_autofocus = 1

    " UltiSnips
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<c-b>"
    let g:UltiSnipsJumpBackwardTrigger="<c-z>"

    " CtrlP
    let g:ctrlp_working_path_mode = 'c'

    " Neomake
    let g:neomake_open_list = 2

    " nvim-completion-manager
    let g:cm_matcher = {
                \ 'module': 'cm_matchers.fuzzy_matcher',
                \ 'case': 'smartcase'
                \}
    let g:cm_refresh_default_min_word_len=2


    let g:LanguageClient_serverCommands = {
                \ 'haskell': ['hie', '--lsp'],
                \ 'cpp': ['cquery']
                \ }
    let g:LanguageClient_loadSettings = 1
    let g:LanguageClient_settingsPath = expand('~/.config/nvim/settings.json')
endfunction

function! BindHaskell()
    au FileType haskell nmap <Leader>d :call LanguageClient_textDocument_definition()<CR>
    au FileType haskell nmap <Leader>h :call LanguageClient_textDocument_hover()<CR>
endfunction

function! BindGo()
    au FileType go nmap <Leader>d <Plug>(go-def)
    au FileType go nmap <Leader>r <Plug>(go-run)<CR>
    au FileType go nmap <leader>b <Plug>(go-build)<CR>
    au FileType go nmap <Leader>t <Plug>(go-test)<CR>
    au FileType go nmap <leader>c <Plug>(go-coverage)<CR>
    au FileType go nmap <Leader>h <Plug>(go-info)<CR>
    au FileType go nmap <Leader>i <Plug>(go-implements)<CR>
endfunction

function! BindKeys()
    let g:go_fmt_command = "goimports"

    nmap <C-b> <Esc>:BufExplorer<cr>
    vmap <C-b> <esc>:BufExplorer<cr>
    imap <C-b> <esc><esc>:BufExplorer<cr>
    nmap <silent> <Leader>A :FSHere<cr>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NERDTreeToggle<CR>
    nmap <Leader>f :NERDTreeFind<CR>
    nmap <f12> :Autoformat<CR>
    nmap <silent> <Leader>b :Neomake!<cr>
    nmap <Leader>t :CtrlP<CR>
    nmap fd :call fzf#vim#ag(expand('<cword>'))<cr>
endfunction

function! BindCpp()
    au FileType cpp nmap gd :call LanguageClient_textDocument_definition()<CR>
    au FileType cpp nmap gh :call LanguageClient_textDocument_hover()<CR>
    autocmd BufWrite *.cpp,*.hpp,*.c,*.h :Autoformat
endfunction

call plug#begin()
Plug 'roxma/nvim-completion-manager'
Plug 'jlanzarotta/bufexplorer'
Plug 'derekwyatt/vim-fswitch'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'majutsushi/tagbar'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'nsf/gocode', {
            \ 'rtp': 'nvim',
            \ 'do': '~/.config/nvim/plugged/gocode/nvim/symlink.sh'
            \ }
Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': './install.sh'
            \ }
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'Chiel92/vim-autoformat'
Plug 'neomake/neomake'
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'tpope/vim-fugitive'
call plug#end()

call BindKeys()
call ConfigureView()
call BindCpp()
call BindHaskell()
call BindGo()
call InitExternalPlugins()

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
    set colorcolumn=88
    set completeopt-=preview
    set cmdheight=1
    let mapleader = "\\"

    set t_Co=256
    set background=light
    colorscheme PaperColor
endfunc

function! InitExternalPlugins()
    " TagBar
    let g:tagbar_left = 1
    let g:tagbar_autofocus = 1

    "" Neomake
    let g:neomake_open_list = 2

    " NERDCommenter
    let g:NERDSpaceDelims = 1

    " completion-vim
    set completeopt=menuone,noinsert,noselect
    set shortmess+=c
    autocmd BufEnter * lua require'completion'.on_attach()
    let g:completion_enable_snippet = 'UltiSnips'
    let g:completion_matching_smart_case = 1
    let g:completion_trigger_keyword_length = 2

    autocmd BufWrite *.go,*.cpp,*.hpp,*.c,*.h :Autoformat

    autocmd FileType markdown,plaintex setlocal wrap


    let g:airline#extensions#branch#displayed_head_limit = 10
    let g:airline#extensions#branch#format = 1

    " vim-cpp-enhanced-highlight
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1

    " for vim-markdown-toc generator
    let g:vmt_dont_insert_fence = 1

    if !empty($DISABLE_ALE)
        let g:ale_linters = {'c': []}
        let g:ale_linters = {'cpp': []}
    endif
endfunction

function! BindKeys()
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gI    <cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
    nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
    nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
    nnoremap <silent> ga    <cmd>lua vim.lsp.buf.code_action()<CR>

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

    inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
endfunction

function! Spelling()
    autocmd BufRead,BufNewFile *.md setlocal spell
    autocmd BufRead,BufNewFile *.txt setlocal spell
    autocmd FileType gitcommit setlocal spell
    set complete+=kspell
endfunc

call plug#begin()
Plug 'NLKNguyen/papercolor-theme'

" general view, edits and navigation
Plug 'majutsushi/tagbar'    " install universal (!!!) ctags
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', {
            \ 'dir': '~/.fzf',
            \ 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'

" LSP support
Plug 'neovim/nvim-lspconfig'
Plug 'nvim-lua/completion-nvim'
Plug 'dense-analysis/ale'

" snippets
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-fugitive'
Plug 'honza/vim-snippets'

" extra languages
Plug 'ekalinin/Dockerfile.vim'
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'mzlogin/vim-markdown-toc'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" programming language toolings
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'jiangmiao/auto-pairs'
Plug 'ludovicchabant/vim-gutentags' " generates ctags for fzf
Plug 'neomake/neomake'
Plug 'Chiel92/vim-autoformat'
Plug 'fatih/vim-go'
Plug 'derekwyatt/vim-fswitch'       " cpp <-> h file switch
call plug#end()

call BindKeys()
call ConfigureView()
call InitExternalPlugins()
call Spelling()

lua << EOF
require'lspconfig'.clangd.setup{}
require'lspconfig'.pyls.setup{}
require'lspconfig'.hls.setup{}
require'lspconfig'.gopls.setup{}
require'lspconfig'.dockerls.setup{}
require'lspconfig'.hls.setup{}
local nvim_lsp = require('lspconfig')
local servers = {
    'clangd'
    }
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {
        handlers = {['textDocument/publishDiagnostics'] = function(...) end  }
        }
end
EOF


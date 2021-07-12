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

    " compe
    let g:compe = {}
    let g:compe.enabled = v:true
    let g:compe.autocomplete = v:true
    let g:compe.debug = v:false
    let g:compe.min_length = 1
    let g:compe.preselect = 'enable'
    let g:compe.throttle_time = 80
    let g:compe.source_timeout = 200
    let g:compe.resolve_timeout = 800
    let g:compe.incomplete_delay = 400
    let g:compe.max_abbr_width = 100
    let g:compe.max_kind_width = 100
    let g:compe.max_menu_width = 100
    let g:compe.documentation = v:true

    let g:compe.source = {}
    let g:compe.source.path = v:true
    let g:compe.source.buffer = v:true
    let g:compe.source.calc = v:true
    let g:compe.source.nvim_lsp = v:true
    let g:compe.source.nvim_lua = v:true
    let g:compe.source.ultisnips = v:true
    let g:compe.source.luasnip = v:true
    let g:compe.source.emoji = v:true

    autocmd BufWrite *.go,*.cpp,*.hpp,*.c,*.h,*.py :Autoformat

    autocmd FileType markdown,plaintex setlocal wrap


    let g:airline#extensions#branch#displayed_head_limit = 10
    let g:airline#extensions#branch#format = 1

    " vim-cpp-enhanced-highlight
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1

    " for vim-markdown-toc generator
    let g:vmt_dont_insert_fence = 1

    " fzf view and bindings
    let g:fzf_preview_window = ['up:50%']
    command! -bang -nargs=? -complete=dir Files
                \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

    " per-folder optional configuration. Usefull for `makeprg` initialization
    " and other small project-dependent changes. For example:
    " set makeprg=bazel\ build\ //...
    if filereadable(".settings/config.vim")
        source .settings/config.vim
    endif
endfunction

function! BindKeys()
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gI    <cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
    nnoremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
    nnoremap <silent> ga    <cmd>lua vim.lsp.buf.code_action()<CR>
    nnoremap <silent> ge    <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>
    nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<CR>

    map <Leader>S <esc>:WorkspaceSymbols
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

    inoremap <silent><expr> <C-Space> compe#complete()
    inoremap <silent><expr> <CR>      compe#confirm(luaeval("require 'nvim-autopairs'.autopairs_cr()"))
    inoremap <silent><expr> <C-e>     compe#close('<C-e>')
    inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
    inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
endfunction

function! Spelling()
    autocmd BufRead,BufNewFile *.md setlocal spell
    autocmd BufRead,BufNewFile *.txt setlocal spell
    autocmd FileType gitcommit setlocal spell
    set complete+=kspell
endfunc

function! s:list_buffers()
    redir => list
    silent ls
    redir END
    return split(list, "\n")
endfunction

function! s:delete_buffers(lines)
    execute 'bwipeout' join(map(a:lines, {_, line -> split(line)[0]}))
endfunction

command! BD call fzf#run(fzf#wrap({
            \ 'source': s:list_buffers(),
            \ 'sink*': { lines -> s:delete_buffers(lines) },
            \ 'options': '--multi --reverse --bind ctrl-a:select-all+accept'
            \ }))

" have to install:
" * universal (!!!) ctags: https://packages.ubuntu.com/search?keywords=universal-ctags
" * ripgrep: https://packages.ubuntu.com/search?keywords=ripgrep
" * bat: https://github.com/sharkdp/bat
" * LSPs:
"   * Bash: npm i -g bash-language-server
"   * C++: sudo aptitude install clangd
"   * Python (pyls): pip install 'python-language-server[all]'
"   * Docker: sudo npm install -g dockerfile-language-server-nodejs
" * PlantUML:
"   * PlantUML: sudo apt-get install -y plantuml
"   * GraphViz: sudo apt-get install graphviz

call plug#begin()
Plug 'NLKNguyen/papercolor-theme'

" general view, edits and navigation
Plug 'majutsushi/tagbar'    " install universal (!!!) ctags
Plug 'vim-airline/vim-airline'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-surround'
Plug 'windwp/nvim-autopairs'
Plug 'octol/vim-cpp-enhanced-highlight'
" PlantUML
Plug 'aklt/plantuml-syntax'
Plug 'tyru/open-browser.vim'
Plug 'weirongxu/plantuml-previewer.vim'

" LSP support
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'gfanto/fzf-lsp.nvim'

" snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" extra languages
Plug 'ekalinin/Dockerfile.vim'
Plug 'martinda/Jenkinsfile-vim-syntax'
Plug 'mzlogin/vim-markdown-toc'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" programming language toolings
Plug 'tpope/vim-fugitive'
Plug 'neomake/neomake'
Plug 'Chiel92/vim-autoformat'
Plug 'fatih/vim-go'
Plug 'derekwyatt/vim-fswitch'       " cpp <-> h file switch
call plug#end()

call BindKeys()
call ConfigureView()
call InitExternalPlugins()
call Spelling()

" pyls expects `~/.config/flake8` as global setup
lua << EOF
require'lspconfig'.clangd.setup{}
require'lspconfig'.pyls.setup{
settings = {
    pyls = {
        configurationSources = { "flake8" }
        }
    }
}
require'lspconfig'.hls.setup{}
require'lspconfig'.gopls.setup{}
require'lspconfig'.bashls.setup{
filetypes={"sh", "zsh"}
}
require'lspconfig'.dockerls.setup{}
require'nvim-autopairs'.setup{}
require'nvim-autopairs.completion.compe'.setup({
  map_cr = true, --  map <CR> on insert mode
  map_complete = true -- it will auto insert `(` after select function or method item
})
require'fzf_lsp'.setup()
EOF


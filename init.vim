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
    set completeopt=menu,menuone,noselect
    set cmdheight=1
    let mapleader = "\\"

    set t_Co=256
    set background=light
    colorscheme nord

    " jump to the last known cursor position on file read
    if has("autocmd")
        au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
    endif
endfunc

function! InitExternalPlugins()
    " TagBar
    let g:tagbar_left = 1
    let g:tagbar_autofocus = 1

    "" Neomake
    let g:neomake_open_list = 2

    " NERDCommenter
    let g:NERDSpaceDelims = 1

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

    map <Leader>b <esc>:Buffers<cr>
    map <Leader>p <esc>:Files<cr>
    map <Leader>t <esc>:Tags<cr>
    map <silent> <Leader>s :Rg <C-R><C-W><CR>
    nmap <silent> <Leader>A :ClangdSwitchSourceHeader<cr>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NERDTreeToggle<CR>
    nmap <Leader>f :NERDTreeFind<CR>
    nmap <f12> :Autoformat<CR>
    nmap <silent> <Leader>C :Neomake!<cr>


    " vim-vsnip
    imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
    smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
    imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
    smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

    " relative path  (src/foo.txt)
    nnoremap <leader>cf :let @+=expand("%")<CR>
    " absolute path  (/something/src/foo.txt)
    nnoremap <leader>cF :let @+=expand("%:p")<CR>
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
" * pip install --upgrade pynvim
" * sudo apt install xclip
" * universal (!!!) ctags: https://packages.ubuntu.com/search?keywords=universal-ctags
" * ripgrep: https://packages.ubuntu.com/search?keywords=ripgrep
" * bat: https://github.com/sharkdp/bat
"   * to avoid installation error on Ubuntu:
"   sudo apt install -o Dpkg::Options::="--force-overwrite" bat ripgrep
" * LSPs:
"   * Bash: sudo npm i -g bash-language-server
"   * C++: sudo aptitude install clangd-12 clang-format-12
"   * Python (pylsp): pip install 'python-lsp-server[all]'
"   * Docker: sudo npm install -g dockerfile-language-server-nodejs
"   * gopls: https://github.com/golang/tools/blob/master/gopls/README.md
"       * :GoInstallBinaries
"   * elixir-ls: https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#elixirls
" * PlantUML:
"   * PlantUML: sudo apt-get install -y plantuml graphviz

call plug#begin()
    Plug 'arcticicestudio/nord-vim'

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
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'hrsh7th/nvim-cmp'

    " snippets
    Plug 'hrsh7th/cmp-vsnip'
    Plug 'hrsh7th/vim-vsnip'
    Plug 'rafamadriz/friendly-snippets'

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
    Plug 'elixir-editors/vim-elixir'
call plug#end()

call BindKeys()
call ConfigureView()
call InitExternalPlugins()
call Spelling()


lua << EOF

local cmp = require'cmp'
cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },

    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'vsnip' },
        { name = 'buffer' },
    }, {
        { name = 'buffer' },
    })
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
        { name = 'buffer' },
    })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

require'lspconfig'.clangd.setup{
    cmd = {
        "clangd",
        "--background-index",
        "-j=12",
    },
    capabilities = capabilities
}
-- pyls expects `~/.config/flake8` as global setup
require'lspconfig'.pylsp.setup{
    settings = {
        pylsp = {
            configurationSources = { "flake8" }
        }
    },
    capabilities = capabilities
}
require'lspconfig'.gopls.setup{
    capabilities = capabilities
}
require'lspconfig'.bashls.setup{
    filetypes={"sh", "zsh"},
    capabilities = capabilities
}
require'lspconfig'.dockerls.setup{
    capabilities = capabilities
}
require'nvim-autopairs'.setup{
    capabilities = capabilities
}

EOF

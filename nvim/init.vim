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

    autocmd BufWritePre *.go,*.cpp,*.hpp,*.c,*.cc,*.h,*.py,*el,BUILD silent! :Neoformat
    let g:neoformat_enabled_python = ['isort', 'black']

    autocmd FileType markdown,plaintex setlocal wrap

    let g:airline#extensions#branch#displayed_head_limit = 10
    let g:airline#extensions#branch#format = 1

    " vim-cpp-enhanced-highlight
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1

    " for vim-markdown-toc generator
    let g:vmt_dont_insert_fence = 1

    " per-folder optional configuration. Usefull for `makeprg` initialization
    " and other small project-dependent changes. For example:
    " set makeprg=bazel\ build\ //...
    if filereadable(".settings/config.vim")
        source .settings/config.vim
    endif
endfunction

function! BindKeys()
    nnoremap <silent> gd    :lua require('telescope.builtin').lsp_definitions()<CR>
    nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
    nnoremap <silent> gI    :lua require('telescope.builtin').lsp_implementations()<CR>
    nnoremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
    nnoremap <silent> gr    :lua require('telescope.builtin').lsp_references()<CR>
    nnoremap <silent> g0    :lua require('telescope.builtin').document_symbols()<CR>
    nnoremap <silent> gW    :lua require('telescope.builtin').workspace_symbols()<CR>
    nnoremap <silent> ga    :lua vim.lsp.buf.code_action()<CR>
    nnoremap <silent> ge    :lua vim.diagnostic.open_float()<CR>
    nnoremap <silent> gR    <cmd>lua vim.lsp.buf.rename()<CR>

    nnoremap <leader>p <cmd>Telescope find_files<cr>
    nnoremap <leader>s :lua require('telescope.builtin').grep_string { search = vim.fn.expand("<cword>") }<CR>
    nnoremap <leader>l <cmd>Telescope live_grep<cr>
    nnoremap <leader>b <cmd>Telescope buffers<cr>
    nnoremap <leader>t <cmd>Telescope help_tags<cr>

    nmap <silent> <Leader>A :ClangdSwitchSourceHeader<cr>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NvimTreeToggle<CR>
    nmap <Leader>f :NvimTreeFindFile<CR>
    nmap <f12> :Neoformat<CR>
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

" have to install:
" * pip install --upgrade pynvim
" * sudo apt install xclip
" * universal (!!!) ctags: https://packages.ubuntu.com/search?keywords=universal-ctags
" * ripgrep: https://packages.ubuntu.com/search?keywords=ripgrep
" * fd: > sudo apt install fd-find
"       > ln -s $(which fdfind) ~/.local/bin/fd
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
    Plug 'windwp/nvim-autopairs'
    Plug 'octol/vim-cpp-enhanced-highlight'
    Plug 'nvim-tree/nvim-web-devicons'
    Plug 'nvim-tree/nvim-tree.lua'
    Plug 'machakann/vim-sandwich'

    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-ui-select.nvim'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

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
    Plug 'sbdchd/neoformat'
    Plug 'fatih/vim-go'
    Plug 'elixir-editors/vim-elixir'
    Plug'dijkstracula/vim-plang'
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
local capabilities = require('cmp_nvim_lsp').default_capabilities()

require'lspconfig'.clangd.setup{
    cmd = {
        "clangd",
        "--background-index",
        "-j=12",
        "--query-driver=**",
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

require'lspconfig'.elixirls.setup{
    cmd = { os.getenv( "HOME" ) .. "/.config/elixir-ls/language_server.sh" };
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

local actions = require "telescope.actions"

require('telescope').setup{
defaults = {
    layout_strategy = 'vertical',
    layout_config = { height = 0.95 },
    },
  pickers = {
    buffers = {
      ignore_current_buffer = true,
      sort_lastused = true,
      mappings = {
        i = {
          ["<c-d>"] = actions.delete_buffer,
        }
      }
    }
  }
}
require('telescope').load_extension 'ui-select'

require'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { "c", "cpp", "python", "go", "bash", "java", "json", "elixir", "erlang" },
    highlight = {
        -- `false` will disable the whole extension
        enable = true,
        },
    }

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("nvim-tree").setup({
  on_attach = on_attach,
  sort_by = "case_sensitive",
  view = {
    adaptive_size = false,
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = false,
  },
  git = {
    enable = true,
    ignore = false,
  },
})

EOF

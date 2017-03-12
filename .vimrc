function! ConfigureView()
    set shell=/bin/sh
    set number
    syntax on
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
    "set lines=45
    "set columns=125
    set iskeyword=@,48-57,_,192-255
    set backspace=indent,eol,start
    set cursorline
    highlight CursorLine guibg=lightblue ctermbg=lightgray
    highlight CursorLine term=none cterm=none
    au FileType qf call AdjustWindowHeight(3, 5)
    set history=200
    set wildmenu
    set list listchars=tab:→\ ,trail:·
    filetype plugin on
    set colorcolumn=80
    set completeopt-=preview
endfunc

function! s:my_cr_function()
    " For no inserting <CR> key.
    return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction

function! InitExternalPlugins()
    " TagBar
    let g:tagbar_left = 1
    let g:tagbar_autofocus = 1

    " UltiSnips
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<c-b>"
    let g:UltiSnipsJumpBackwardTrigger="<c-z>"

    " NeoComplete
    let g:acp_enableAtStartup = 0
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#sources#syntax#min_keyword_length = 2
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

    if !exists('g:neocomplete#keyword_patterns')
        let g:neocomplete#keyword_patterns = {}
    endif
    let g:neocomplete#keyword_patterns['default'] = '\h\w*'

    if !exists('g:neocomplete#force_omni_input_patterns')
        let g:neocomplete#force_omni_input_patterns = {}
    endif
    let g:neocomplete#force_omni_input_patterns.go = '[^.[:digit:] *\t]\.'

    let g:neocomplete#enable_auto_select = 1
    let g:neocomplete#same_filetypes = {}
    let g:neocomplete#same_filetypes._ = '_'

    let g:ctrlp_working_path_mode = 'c'

    let g:go_fmt_command = "goimports"

    let g:ackprg = 'ag --vimgrep -U'

    let g:vim_tags_auto_generate = 1
    let g:vim_tags_ignore_files = []

    let g:tagbar_type_haskell = {
                \ 'ctagsbin'  : 'hasktags',
                \ 'ctagsargs' : '-x -c -o-',
                \ 'kinds'     : [
                \  'm:modules:0:1',
                \  'd:data: 0:1',
                \  'd_gadt: data gadt:0:1',
                \  't:type names:0:1',
                \  'nt:new types:0:1',
                \  'c:classes:0:1',
                \  'cons:constructors:1:1',
                \  'c_gadt:constructor gadt:1:1',
                \  'c_a:constructor accessors:1:1',
                \  'ft:function types:1:1',
                \  'fi:function implementations:0:1',
                \  'o:others:0:1'
                \ ],
                \ 'sro'        : '.',
                \ 'kind2scope' : {
                \ 'm' : 'module',
                \ 'c' : 'class',
                \ 'd' : 'data',
                \ 't' : 'type'
                \ },
                \ 'scope2kind' : {
                \ 'module' : 'm',
                \ 'class'  : 'c',
                \ 'data'   : 'd',
                \ 'type'   : 't'
                \ }
                \ }
    let g:haskellmode_completion_ghc = 1
    autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc
endfunction

function! BindKeys()
    " просмотр списка буферов
    nmap <C-b> <Esc>:BufExplorer<cr>
    vmap <C-b> <esc>:BufExplorer<cr>
    imap <C-b> <esc><esc>:BufExplorer<cr>
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NERDTreeToggle<CR>
    nmap <Leader>f :NERDTreeFind<CR>
    nmap <Leader>t :CommandT<CR>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <silent> <Leader>A :FSHere<cr>
    cnoreabbrev Ack Ack!
    nmap fd :Ack<Space>
    nmap <f12> :Autoformat<CR>
    inoremap <expr><C-g>     neocomplete#undo_completion()
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
    nmap <silent> <Leader>b :Make<cr>
    nmap <silent> <Leader>T :TagsGenerate!<cr>
endfunction

function! BindGo()
    au FileType go nmap <C-]> <Plug>(go-def)
    au FileType go nmap <leader>gr <Plug>(go-run)
    au FileType go nmap <leader>gb <Plug>(go-build)
    au FileType go nmap <leader>gt <Plug>(go-test)
    au FileType go nmap <leader>gc <Plug>(go-coverage)
    au FileType go nmap <Leader>gs <Plug>(go-implements)
    au FileType go nmap <Leader>gi <Plug>(go-info)
endfunction

function! LocalConf()
    if filereadable(".vim_config")
        source .vim_config
    endif
endfunc

function! Spelling()
    autocmd BufRead,BufNewFile *.md setlocal spell
    autocmd BufRead,BufNewFile *.txt setlocal spell
    autocmd FileType gitcommit setlocal spell
    set complete+=kspell
endfunc


function! BindFileTypes()
    au BufRead,BufNewFile *.mm set filetype=cpp
    au BufRead,BufNewFile *.proto set filetype=proto
    au BufRead,BufNewFile *.protoc set filetype=proto
endfunc

function! AdjustWindowHeight(minheight, maxheight)
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/neoinclude.vim'
Plugin 'Shougo/vimproc.vim' " VimProcInstall
Plugin 'majutsushi/tagbar'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'derekwyatt/vim-fswitch'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'wincent/command-t'  " cd ~/.vim/bundle/command-t/ruby/command-t
" ruby extconf.rb; make
Plugin 'szw/vim-tags'
Plugin 'fatih/vim-go'
Plugin 'mileszs/ack.vim' " brew install the_silver_searcher
Plugin 'tpope/vim-fugitive'
Bundle 'cespare/vim-toml'
Plugin 'tpope/vim-dispatch'
Plugin 'chrisbra/vim-diff-enhanced'
Plugin 'davidhalter/jedi-vim'
Plugin 'freitass/todo.txt-vim'
Bundle 'uarun/vim-protobuf'
Bundle 'jiangmiao/auto-pairs'
Bundle 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'eagletmt/ghcmod-vim'
" cabal install ghc-mod
" cabal install hasktags
Plugin 'eagletmt/neco-ghc'
Plugin 'Chiel92/vim-autoformat'
" cabal install stylish-haskell
" brew install clang-format
call vundle#end()
filetype plugin indent on

call LocalConf()
call ConfigureView()
call BindKeys()
call InitExternalPlugins()
call BindFileTypes()
call BindGo()
call Spelling()

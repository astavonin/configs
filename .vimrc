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
    set statusline=%<%f%h%m%r\ %b\ %{&encoding}\ 0x\ \ %l,%c%V\ %P
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
    let g:tagbar_left = 1
    let g:UltiSnipsExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<c-b>"
    let g:UltiSnipsJumpBackwardTrigger="<c-z>"

    let g:acp_enableAtStartup = 0
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#sources#syntax#min_keyword_length = 3
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

    let g:ag_working_path_mode="r"

    let g:go_fmt_command = "goimports"
endfunction

function! BindKeys()
    " просмотр списка буферов
    nmap <C-b> <Esc>:BufExplorer<cr>
    vmap <C-b> <esc>:BufExplorer<cr>
    imap <C-b> <esc><esc>:BufExplorer<cr>
    nmap <C-\> :TagbarToggle<CR>
    nmap <Leader>F :NERDTreeToggle<CR>
    cnoremap @ <c-r>=expand("%:h")<cr>/
    nmap <silent> <Leader>A :FSHere<cr>
    nmap fd :Ag<cr>
    inoremap <expr><C-g>     neocomplete#undo_completion()
    inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
    inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
    autocmd FileType c,cpp,objc nnoremap <buffer><Leader>C :<C-u>ClangFormat<CR>
    autocmd FileType c,cpp,objc vnoremap <buffer><Leader>C :ClangFormat<CR>
    nmap <silent> <Leader>b :Make<cr>
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

function! BindFileTypes()
    au BufRead,BufNewFile *.mm set filetype=cpp
endfunc

function! InitClangFormat()
    let g:clang_format#style_options = {
                \ "AlignConsecutiveAssignments" : "true",
                \ "AlignConsecutiveDeclarations" : "true",
                \ "AllowShortFunctionsOnASingleLine" : "false",
                \ "SpaceBeforeParens" : "Never",
                \ "SpacesInParentheses" : "true",
                \ "UseTab" : "Never",
                \ "AccessModifierOffset" : -4,
                \ "AlwaysBreakTemplateDeclarations" : "true",
                \ "Standard" : "C++11",
                \ "BreakConstructorInitializersBeforeComma" : "true",
                \ "BreakBeforeBraces" : "Allman"}
endfunct

au FileType qf call AdjustWindowHeight(3, 5)
function! AdjustWindowHeight(minheight, maxheight)
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Shougo/neocomplete.vim'
Plugin 'majutsushi/tagbar'
Plugin 'jlanzarotta/bufexplorer'
Plugin 'derekwyatt/vim-fswitch'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'wincent/command-t'  " cd ~/.vim/bundle/command-t/ruby/command-t
                            " ruby extconf.rb; make
Plugin 'szw/vim-tags'
Plugin 'fatih/vim-go'
Plugin 'rking/ag.vim'   " brew install the_silver_searcher
Plugin 'tpope/vim-fugitive'
Bundle 'cespare/vim-toml'
Plugin 'tpope/vim-dispatch'
Plugin 'chrisbra/vim-diff-enhanced'
Plugin 'davidhalter/jedi-vim'
Plugin 'rhysd/vim-clang-format'
Plugin 'freitass/todo.txt-vim'

call vundle#end()
filetype plugin indent on


call LocalConf()
call ConfigureView()
call BindKeys()
call InitExternalPlugins()
call BindFileTypes()
call BindGo()
call InitClangFormat()

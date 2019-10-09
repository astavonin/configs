function! ConfigureView()
	"set shell=/bin/sh
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
	" TagBar
	let g:tagbar_left = 1
	let g:tagbar_autofocus = 1

	" UltiSnips shouldn't break Coc bindings
	let g:UltiSnipsExpandTrigger="<C-j>"

	"" Neomake
	let g:neomake_open_list = 2

	" NERDCommenter
	let g:NERDSpaceDelims = 1

	let g:go_fmt_command = "goimports"
	let g:go_def_mapping_enabled = 0

	autocmd BufWrite *.go,*.cpp,*.hpp,*.c,*.h :Autoformat

	let g:tagbar_type_elixir = {
	    \ 'ctagstype' : 'elixir',
	    \ 'kinds' : [
	        \ 'p:protocols',
	        \ 'm:modules',
	        \ 'e:exceptions',
	        \ 'y:types',
	        \ 'd:delegates',
	        \ 'f:functions',
	        \ 'c:callbacks',
	        \ 'a:macros',
	        \ 't:tests',
	        \ 'i:implementations',
	        \ 'o:operators',
	        \ 'r:records'
	    \ ],
	    \ 'sro' : '.',
	    \ 'kind2scope' : {
	        \ 'p' : 'protocol',
	        \ 'm' : 'module'
	    \ },
	    \ 'scope2kind' : {
	        \ 'protocol' : 'p',
	        \ 'module' : 'm'
	    \ },
	    \ 'sort' : 0
	\ }
endfunction


function! BindKeys()
	nmap <silent> <Leader>d <Plug>(coc-definition)
	nmap <silent> <Leader>t <Plug>(coc-type-definition)
	nmap <silent> <Leader>i <Plug>(coc-implementation)
	nmap <silent> <Leader>r <Plug>(coc-references)
	nmap <leader>R <Plug>(coc-rename)
	nnoremap <silent> <Leader>s  :<C-u>CocList -I symbols<cr>
	nmap <silent> <Leader>[ <Plug>(coc-diagnostic-prev)
	nmap <silent> <Leader>] <Plug>(coc-diagnostic-next)

	nmap <C-b> <Esc>:BufExplorer<cr>
	vmap <C-b> <esc>:BufExplorer<cr>
	imap <C-b> <esc><esc>:BufExplorer<cr>
	" nmap <silent> <Leader>A :FSHere<cr>
	cnoremap @ <c-r>=expand("%:h")<cr>/
	nmap <C-\> :TagbarToggle<CR>
	nmap <Leader>F :NERDTreeToggle<CR>
	nmap <Leader>f :NERDTreeFind<CR>
	nmap <f12> :Autoformat<CR>
	nmap <silent> <Leader>b :Neomake!<cr>
	inoremap <silent><expr> <Tab>
				\ pumvisible() ? "\<C-n>" :
				\ <SID>check_back_space() ? "\<Tab>" :
				\ coc#refresh()

	nnoremap <silent> <Leader>a :exe 'CocList -I -A --input='.expand('<cword>').' grep'<CR>
	nnoremap <silent> <Leader>p :exe 'CocList --input='.expand('<cword>').' files'<CR>
endfunction

function! Spelling()
	autocmd BufRead,BufNewFile *.md setlocal spell
	autocmd BufRead,BufNewFile *.txt setlocal spell
	autocmd FileType gitcommit setlocal spell
	set complete+=kspell
endfunc

function! s:check_back_space() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~ '\s'
endfunction

"imap <C-l> <Plug>(coc-snippets-expand)
"let g:coc_snippet_next = '<c-j>'
"let g:coc_snippet_prev = '<c-k>'


call plug#begin()
Plug 'NLKNguyen/papercolor-theme'

Plug 'jlanzarotta/bufexplorer'
Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdcommenter'
Plug 'scrooloose/nerdtree'

Plug 'Chiel92/vim-autoformat'
Plug 'neomake/neomake'
Plug 'tpope/vim-fugitive'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'fatih/vim-go'
Plug 'elixir-editors/vim-elixir'
call plug#end()

call BindKeys()
call ConfigureView()
call InitExternalPlugins()
call Spelling()

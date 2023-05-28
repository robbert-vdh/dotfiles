" No compatibility to traditional vi
set nocompatible

" True colors for most modern terminal emulators
set termguicolors

" Add plugins
call plug#begin('~/.config/nvim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Interface
Plug 'airblade/vim-gitgutter'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'easymotion/vim-easymotion'

" Workflow enhancements
Plug 'ervandew/supertab'
Plug 'godlygeek/tabular'
Plug 'jiangmiao/auto-pairs'

" Syntax
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'cakebaker/scss-syntax.vim'
Plug 'cespare/vim-toml'
Plug 'dag/vim-fish'
Plug 'editorconfig/editorconfig-vim'
Plug 'lepture/vim-jinja'
Plug 'othree/html5.vim'
Plug 'plasticboy/vim-markdown'
Plug 'stephpy/vim-yaml'

call plug#end()

" Syntax highlighting
syntax on

" Softtab -- use spaces instead tabs
set expandtab
set tabstop=2 shiftwidth=2 sts=2
set autoindent nosmartindent

" Set tab characters apart
set listchars=tab:↹\

" I dislike CRLF
if !exists("vimpager")
  set fileformat=unix
endif

set backspace=2

" Detect modeline hints
set modeline

" Disable bell
set visualbell t_vb=

" Prefer UTF-8
set encoding=utf-8 fileencodings=ucs-bom,utf-8,cp949,korea,iso-2022-kr

" More tabs
set tabpagemax=25

filetype plugin on

" Some additional syntax highlighters
au! BufRead,BufNewFile *.wsgi setfiletype python
au! BufRead,BufNewFile *.sass setfiletype sass
au! BufRead,BufNewFile *.scss setfiletype scss
au! BufRead,BufNewFile *.haml setfiletype haml
au! BufRead,BufNewFile *.less setfiletype less

" These languages have their own tab/indent settings
au FileType cpp    setl ts=2 sw=2 sts=2
au FileType ruby   setl ts=2 sw=2 sts=2
au FileType yaml   setl ts=2 sw=2 sts=2
au FileType html   setl ts=2 sw=2 sts=2
au FileType jinja  setl ts=2 sw=2 sts=2
au FileType lua    setl ts=2 sw=2 sts=2
au FileType haml   setl ts=2 sw=2 sts=2
au FileType sass   setl ts=2 sw=2 sts=2
au FileType scss   setl ts=2 sw=2 sts=2
au FileType make   setl ts=4 sw=4 sts=4 noet
au FileType gitcommit setl spell

" Markdown-related configurations
augroup mkd
  autocmd BufRead *.markdown set formatoptions=tcroqn2 comments=n:> spell
  autocmd BufRead *.mkdn     set formatoptions=tcroqn2 comments=n:> spell
  autocmd BufRead *.mkd      set formatoptions=tcroqn2 comments=n:> spell
augroup END

" English and Dutch spelling checker
setlocal spelllang=en_gb,nl

" Use the same exchange.vim keybindings as evil-exchange
let g:exchange_no_mappings=1
nmap gx <Plug>(Exchange)
nmap gxx <Plug>(ExchangeLine)
nmap gX <Plug>(ExchangeClear)

" Same for easymotion
map gs <Plug>(easymotion-prefix)

" Show a ruler at 80 columns
set colorcolumn=80
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/
autocmd WinEnter * match OverLength /\%81v.\+/

" Vim-airline
let g:airline_powerline_fonts = 1

" Colorscheme
" Not needed since we've already enabled `termguicolors`
let base16colorspace=256
colorscheme base16-tomorrow-night

" Don't automatically fold sections in markdown files
let g:vim_markdown_folding_disabled = 1

" Enable better searching options
set ignorecase smartcase
set incsearch

" Always end files with a newline
set endofline

" Always show line numbers
set number
set relativenumber
" Except for git commits
au FileType gitcommit set nonumber

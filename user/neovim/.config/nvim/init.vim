" No compatibility to traditional vi
set nocompatible

" True colors for most modern terminal emulators
set termguicolors

" Add plugins
call plug#begin('~/.config/nvim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Interface
Plug 'vim-airline/vim-airline'
Plug 'w0rp/ale'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'zchee/deoplete-jedi'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'simnalamburt/vim-mundo'
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimfiler.vim'

" Workflow enhancements
Plug 'jiangmiao/auto-pairs'
Plug 'godlygeek/tabular'
Plug 'ervandew/supertab'

" Syntax
Plug 'editorconfig/editorconfig-vim'
Plug 'othree/html5.vim'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'lepture/vim-jinja'
Plug 'plasticboy/vim-markdown'
Plug 'cakebaker/scss-syntax.vim'
Plug 'cespare/vim-toml'
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

" ALE-related configurations
let g:ale_linters = {
\    'haskell': ['stack-ghc', 'ghc'],
\}

" Markdown-related configurations
augroup mkd
  autocmd BufRead *.markdown set formatoptions=tcroqn2 comments=n:> spell
  autocmd BufRead *.mkdn     set formatoptions=tcroqn2 comments=n:> spell
  autocmd BufRead *.mkd      set formatoptions=tcroqn2 comments=n:> spell
augroup END

" Haskell-related config
let g:haskell_quasi         = 0
let g:haskell_interpolation = 0
let g:haskell_regex         = 0
let g:haskell_jmacro        = 0
let g:haskell_shqq          = 0
let g:haskell_sql           = 0
let g:haskell_json          = 0
let g:haskell_xml           = 0
let g:haskell_hsp           = 0

" English and Dutch spelling checker
setlocal spelllang=en_uk,nl

" Show a ruler at 80 columns
set colorcolumn=80
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/
autocmd WinEnter * match OverLength /\%81v.\+/

" gVim-specific configurations (including MacVim).
if has("gui_running")
  set bg=dark
  set guioptions=egmrLt
  set linespace=1
endif

" MacVim-specific configurations.
if has("gui_macvim") || has("gui_vimr")
  set imd
  set guifont=Source_Code_Pro_Light:h16.00
endif

" GVim under GNOME
if has("gui_gnome")
  set guifont="Ubuntu Mono 11"
endif

" GVim under Windows
if has("gui_win32")
  set guifont=Consolas:h11:cANSI
endif

" Vim-airline
let g:airline_powerline_fonts = 1

" Mundo -- Undo tree visualization
set undofile
set undodir=~/.config/nvim/undo
nnoremap <F5> :MundoToggle

" Use Vimfiler as default explorer like netrw
let g:vimfiler_as_default_explorer = 1

" deoplete
let g:deoplete#enable_at_startup = 1

" VimShell
let g:vimshell_prompt_expr = '$USER . " " . fnamemodify(getcwd(), ":~") . " $ "'
let g:vimshell_prompt_pattern = '^[a-z_-][a-z0-9_-]\{,31\} [~/][^$ ]* $ '

" Colorscheme
" Not needed since we've already enabled `termguicolors`
"let base16colorspace=256
colorscheme base16-tomorrow-night
set background=dark
" Disable the theme's background and simply use the terminal background
highlight Normal guibg=none ctermbg=none
highlight DiffAdded guibg=none ctermbg=none
highlight DiffRemoved guibg=none ctermbg=none

" Don't automatically fold sections in markdown files
let g:vim_markdown_folding_disabled = 1

" Enable better searching options
set smartcase
set incsearch

" Always end files with a newline
set endofline

" Always show line numbers
set number
" Except for git commits
au FileType gitcommit set nonumber

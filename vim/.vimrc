set nocompatible          " get rid of Vi compatibility mode. SET FIRST!

" ----------------------------------------------------------------------------
"  Vundle
" ----------------------------------------------------------------------------

filetype off

" Setting up Vundle - the vim plugin bundler
    let iCanHazVundle=1
    let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
    if !filereadable(vundle_readme)
        echo "Installing Vundle.."
        echo ""
        silent !mkdir -p ~/.vim/bundle
        silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
        let iCanHazVundle=0
    endif
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
    Bundle 'gmarik/vundle'
    "Add your bundles here
"    Bundle 'Syntastic' "uber awesome syntax and errors highlighter
"    Bundle 'altercation/vim-colors-solarized' "T-H-E colorscheme
"    Bundle 'https://github.com/tpope/vim-fugitive' "So awesome, it should be illegal 
    Bundle 'flazz/vim-colorschemes'
	Bundle 'chriskempson/vim-tomorrow-theme'
"...All your other bundles...
    if iCanHazVundle == 0
        echo "Installing Bundles, please ignore key map error messages"
        echo ""
        :BundleInstall
    endif
" Setting up Vundle - the vim plugin bundler end

 "
 " Brief help
 " :BundleList          - list configured bundles
 " :BundleInstall(!)    - install(update) bundles
 " :BundleSearch(!) foo - search(or refresh cache first) for foo
 " :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
 "
 " see :h vundle for more details or wiki for FAQ
 " NOTE: comments after Bundle command are not allowed..



filetype plugin indent on " filetype detection[ON] plugin[ON] indent[ON]
"set tabstop=2             " tab spacing
"set softtabstop=4         " unify
"set shiftround            " always indent/outdent to the nearest tabstop

" ----------------------------------------------------------------------------
"  Text Formatting
" ----------------------------------------------------------------------------

set autoindent             " automatic indent new lines
set smartindent            " be smart about it
inoremap # X<BS>#
"set nowrap                 " do not wrap lines
set softtabstop=2          " yep, two
set shiftwidth=2           " ..
set tabstop=4
set expandtab              " expand tabs to spaces
set nosmarttab             " fuck tabs
set formatoptions+=n       " support for numbered/bullet lists
set textwidth=80           " wrap at 80 chars by default
set virtualedit=block      " allow virtual edit in visual block ..
set binary
set noeol

" ----------------------------------------------------------------------------
"  Remapping
" ----------------------------------------------------------------------------

" lead with ,
let mapleader = ","

" exit to normal mode with 'jj'
inoremap jj <ESC>

" reflow paragraph with Q in normal and visual mode
nnoremap Q gqap
vnoremap Q gq

" sane movement with wrap turned on
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" ----------------------------------------------------------------------------
"  UI
" ----------------------------------------------------------------------------

set t_Co=256              " enable 256-color mode.

set ruler                  " show the cursor position all the time
set showcmd                " display incomplete commands
"set nolazyredraw           " turn off lazy redraw
set number                 " line numbers
set wildmenu               " turn on wild menu
set cursorline
set cursorcolumn
set wildmode=list:longest,full
set ch=2                   " command line height
set backspace=2            " allow backspacing over everything in insert mode
set whichwrap+=<,>,h,l,[,] " backspace and cursor keys wrap to
set shortmess=filtIoOA     " shorten messages
set report=0               " tell us about changes
set nostartofline          " don't jump to the start of line when scrolling
set title
"Set colorscheme
colorscheme Tomorrow-Night-Bright
set scrolloff=3           " Start scrolling three lines before the horizontal window border

" ----------------------------------------------------------------------------
" Visual Cues
" ----------------------------------------------------------------------------

set showmode
set showmatch              " brackets/braces that is
set mat=5                  " duration to show matching brace (1/10 sec)
set incsearch              " do incremental searching
set laststatus=2           " always show the status line
set ignorecase             " ignore case when searching
"set hlsearch               " highlight searches
set visualbell             " shut the fuck up
set nohlsearch             " Don't continue to highlight searched phrases.

" ---------------------------------------------------------------------------
" System Options
" ---------------------------------------------------------------------------

set clipboard=unnamed
set modeline
set modelines=4
set mouse=a
set exrc
set secure
set esckeys
set ttyfast
set gdefault
set encoding=utf-8 nobomb

" ---------------------------------------------------------------------------
"  Strip all trailing whitespace in file
" ---------------------------------------------------------------------------

function! StripWhitespace ()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfunction
map <leader>s :call StripWhitespace ()<CR>

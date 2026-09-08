set nocompatible          " get rid of Vi compatibility mode. SET FIRST!

" ----------------------------------------------------------------------------
"  Vundle
" ----------------------------------------------------------------------------

filetype off

if empty(glob('~/.vim/autoload/plug.vim'))
  if executable('curl')
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  endif
endif

" Plugin installation is explicit: :PlugInstall (or vim +PlugInstall +qall).

call plug#begin('~/.vim/plugged')
Plug 'flazz/vim-colorschemes'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'chriskempson/base16-vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
" Plug 'Shougo/unite.vim'
"Plug 'scrooloose/syntastic'
"Plug 'neomake/neomake'
Plug 'Shougo/vimproc'
" Plug 'Shougo/vimshell'
Plug 'scrooloose/nerdtree'
Plug 'nathanaelkane/vim-indent-guides'
" Plug 'mileszs/ack.vim'
Plug 'jcf/vim-latex'

Plug 'sbdchd/neoformat'

Plug 'Chiel92/vim-autoformat'
Plug 'airblade/vim-gitgutter' 

"Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
" ES2015 code snippets (Optional)
Plug 'epilande/vim-es2015-snippets'
" React code snippets
Plug 'epilande/vim-react-snippets'

" Plug 'einars/js-beautify'
" Plug 'maksimr/vim-jsbeautify'
Plug 'ternjs/tern_for_vim', { 'for': ['javascript', 'javascript.jsx'] }
"Plug 'Yggdroot/indentLine'
Plug 'scrooloose/nerdcommenter'
Plug 'xuhdev/vim-latex-live-preview'
"Plug 'kien/ctrlp.vim'
"Plug 'pangloss/vim-javascript'
"Plug 'mxw/vim-jsx'

Plug 'mbbill/undotree'
let g:undotree_HighlightChangedWithSign = 0
let g:undotree_WindowLayout             = 4
nnoremap <Leader>u :UndotreeToggle<CR>

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --no-bash' }
Plug 'junegunn/fzf.vim'

Plug 'sheerun/vim-polyglot'
Plug 'junegunn/vim-easy-align'
"Plug 'elzr/vim-json'
Plug 'fleischie/vim-styled-components'
Plug 'suan/vim-instant-markdown'
"Plug 'godlygeek/tabular'
"Plug 'plasticboy/vim-markdown'

Plug 'folke/tokyonight.nvim'
call plug#end()



"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..


syntax on
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
set tabstop=2
set expandtab              " expand tabs to spaces
set nosmarttab             " fuck tabs
set formatoptions+=n       " support for numbered/bullet lists
"set textwidth=80           " wrap at 80 chars by default
set textwidth=0           " wrap at 80 chars by default nope
set virtualedit=block      " allow virtual edit in visual block ..
" Messes with tab to spaces, needed so that no newline at EOF
"set binary
"set noeol
"set paste

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

if !has('nvim')
  set t_Co=256             " enable 256-color mode.
endif
set hidden
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
"set background=dark
set scrolloff=3           " Start scrolling three lines before the horizontal window border
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/

set nobackup
set nowb
set noswapfile


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
set mouse=
set exrc
set secure
"set esckeys
"set ttyfast
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

let g:indent_guides_guide_size = 1
let g:tex_flavor='latex'
let g:Tex_DefaultTargetFormat='pdf'
set grepprg=grep\ -nH\ $*

set autoindent             " automatic indent new lines

nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

map <C-n> :NERDTreeToggle<CR>
map <C-m> :NERDTreeFind<CR>

noremap <F8> :Autoformat<CR><CR>
noremap <F7> :Neoformat<CR><CR>

"let g:formatprg_args_expr_cs = '"--mode=cs --style=ansi -pcH".(&expandtab ? "s".&shiftwidth : "t")' 
let g:formatprg_args_expr_c = '"--style=allman --add-brackets --indent=spaces=2 --break-blocks --pad-oper --convert-tabs --delete-empty-lines --quiet"' 
let g:formatprg_args_expr_cpp = '"--style=allman --add-brackets --indent=spaces=2 --break-blocks --pad-oper --convert-tabs --delete-empty-lines --quiet"' 

let g:formatdef_esformatter = '"esformatter"'
let g:formatters_javascript = [
                \ 'esformatter',
                \ 'jsbeautify_javascript',
                \ ]

let g:neoformat_enabled_javascript = ['prettier']

cnoreabbrev <expr> W ((getcmdtype() is# ':' && getcmdline() is# 'W')?('w'):('W'))
cnoreabbrev <expr> Q ((getcmdtype() is# ':' && getcmdline() is# 'Q')?('q'):('Q'))
let g:EclimCompletionMethod = 'omnifunc'
let g:EclimMavenPomClasspathUpdate = 0
let g:Powerline_symbols = 'unicode'

function! GenerateUnicode(first, last)
  let i = a:first
  while i <= a:last
    if (i%256 == 0)
      $put ='----------------------------------------------------'
      $put ='     0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F '
      $put ='----------------------------------------------------'
    endif
    let c = printf('%04X ', i)
    for j in range(16)
      let c = c . nr2char(i) . ' '
      let i += 1
    endfor
    $put =c
  endwhile
endfunction

"cmap w!! w !sudo tee > /dev/null %
"cmap w!! :execute ':silent w !sudo tee % > /dev/null' | :edit!
command WW :execute ':silent w !sudo tee % > /dev/null' | :edit!
let g:ycm_add_preview_to_completeopt=0
let g:ycm_confirm_extra_conf=0
set completeopt-=preview

hi MatchParen cterm=bold ctermbg=none ctermfg=magenta


let g:formatprg_cuda = "astyle" 
let g:formatprg_args_expr_cuda = g:formatprg_args_expr_c 

" Put plugins and dictionaries in this dir (also on Windows)
let vimDir = '$HOME/.vim'

" Keep undo history across sessions by storing it in a file
if has('persistent_undo')
    let myUndoDir = expand('/tmp/undodir')
    " Create dirs
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

set undolevels=1000         " Maximum number of changes that can be undone
set undoreload=10000        " Maximum number lines to save for undo on a buffer reload

function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

hi clear SpellBad
hi SpellBad cterm=bold,italic ctermfg=red

let g:jsx_ext_required = 0

set statusline+=%#warningmsg#
set statusline+=%{exists('g:loaded_syntastic_plugin')?SyntasticStatuslineFlag():''}
" set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_exe = '[ -f $(npm bin)/eslint ] && $(npm bin)/eslint'
let g:syntastic_javascript_eslint_exec = '/bin/ls'

set list

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

if filereadable(expand("~/.vimrc_background"))
  let base16colorspace=256
  source ~/.vimrc_background
endif
"Set colorscheme
"colorscheme base16-eighties
"colorscheme desert
colorscheme tokyonight-night

let g:javascript_plugin_flow = 1
let g:javascript_plugin_jsdoc = 1

"let g:markdown_folding = 1

"noremap <Up>    <Nop>
"noremap <Down>  <Nop>
"noremap <Left>  <Nop>
"noremap <Right> <Nop>

"let g:neomake_javascript_enabled_makers = ['eslint']
"function! NeomakeESlintChecker()
  "let l:npm_bin = ''
  "let l:eslint = 'eslint'

  "if executable('npm-which')
    "let l:eslint = split(system('npm-which eslint'))[0]
    "return 0
  "endif

  "if executable('npm')
    "let l:npm_bin = split(system('npm bin'), '\n')[0]
  "endif

  "if strlen(l:npm_bin) && executable(l:npm_bin . '/eslint')
    "let l:eslint = l:npm_bin . '/eslint'
  "endif

  "let b:neomake_javascript_eslint_exe = l:eslint
"endfunction

"autocmd FileType javascript :call NeomakeESlintChecker()
"autocmd! BufWritePost,BufReadPost * Neomake

nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>

"COC

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config
inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying code actions to the selected code block
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying code actions at the cursor position
nmap <leader>ac  <Plug>(coc-codeaction-cursor)
" Remap keys for apply code actions affect whole buffer
nmap <leader>as  <Plug>(coc-codeaction-source)
" Apply the most preferred quickfix action to fix diagnostic on the current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Remap keys for applying refactor code actions
nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)

" Run the Code Lens action on the current line
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> to scroll float windows/popups
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges
" Requires 'textDocument/selectionRange' support of language server
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

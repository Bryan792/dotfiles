" Load the shared Vim configuration when this optional package is deployed.
" Neovim and its plugins are intentionally deferred on Gura.

if filereadable(expand('~/.vimrc'))
  silent! execute 'source ' . fnameescape(expand('~/.vimrc'))
endif

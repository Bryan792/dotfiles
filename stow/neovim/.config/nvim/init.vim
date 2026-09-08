" Load the shared Vim configuration when this optional package is deployed.
" Neovim and its plugins are intentionally deferred on Gura.

set runtimepath+=~/.vim,~/.vim/after
set packpath+=~/.vim

if filereadable(expand('~/.vimrc'))
  execute 'source ' . fnameescape(expand('~/.vimrc'))
endif

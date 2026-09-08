# Environment shared by login, interactive, and noninteractive Zsh.

export DOTFILES="${DOTFILES:-$HOME/.dotfiles}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"

# Ubuntu's global zshrc otherwise runs compinit before Zim's completion module.
skip_global_compinit=1

typeset -U path PATH
path=(
  "$HOME/.local/bin"
  "$HOME/bin"
  "$DOTFILES/bin"
  $path
)
export PATH

if (( $+commands[nvim] )); then
  export EDITOR="${EDITOR:-nvim}"
  export VISUAL="${VISUAL:-nvim}"
elif (( $+commands[vim] )); then
  export EDITOR="${EDITOR:-vim}"
  export VISUAL="${VISUAL:-vim}"
else
  export EDITOR="${EDITOR:-vi}"
  export VISUAL="${VISUAL:-vi}"
fi

export PAGER="${PAGER:-less}"
export LESS="${LESS:--F -g -i -M -R -S -w -X -z-4}"

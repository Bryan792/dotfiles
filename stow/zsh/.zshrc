[[ -o interactive ]] || return 0

setopt APPEND_HISTORY
setopt COMPLETE_IN_WORD
setopt EXTENDED_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY
setopt PROMPT_SUBST
setopt SHARE_HISTORY
setopt NO_LIST_BEEP
setopt CORRECT

HISTFILE="${HISTFILE:-$HOME/.zsh_history}"
HISTSIZE="${HISTSIZE:-10000}"
SAVEHIST="${SAVEHIST:-10000}"
bindkey -e

# Keep repository functions available without sourcing every legacy .zsh file.
if [[ -d "$DOTFILES/zsh/functions" ]]; then
  fpath=("$DOTFILES/zsh/functions" $fpath)
fi

ZIM_HOME="${ZIM_HOME:-$HOME/.zim}"
if [[ -r "$ZIM_HOME/zimfw.zsh" ]]; then
  if [[ ! -r "$ZIM_HOME/init.zsh" || "$ZIM_HOME/init.zsh" -ot "$HOME/.zimrc" ]]; then
    source "$ZIM_HOME/zimfw.zsh" init -q
  fi
  if [[ -r "$ZIM_HOME/init.zsh" ]] && (( ! ${+_DOTFILES_ZIM_INITIALIZED} )); then
    source "$ZIM_HOME/init.zsh"
    typeset -g _DOTFILES_ZIM_INITIALIZED=1
  fi
fi

if (( ! ${+_DOTFILES_ZIM_INITIALIZED} )); then
  autoload -Uz compinit
  compinit -d "$HOME/.zcompdump"
fi

alias reload='source "$HOME/.zshrc"'
alias ..='cd ..'
alias ...='cd ../..'
alias g='git'
alias h='history'
alias j='jobs'
alias l='ls -lah --color=auto'
alias la='ls -Ahl --color=auto'
alias ll='ls -lh --color=auto'
alias gs='git status -sb'
alias gl='git pull --prune'
alias gd='git diff'
alias gco='git checkout'
alias gb='git branch'

if (( $+commands[nvim] )); then
  alias vi='nvim'
  alias vim='nvim'
elif (( $+commands[vim] )); then
  alias vi='vim'
fi

if (( $+commands[dircolors] )) && [[ -r "$DOTFILES/zsh/dircolors-solarized/dircolors.ansi-universal" ]]; then
  eval "$(dircolors --sh "$DOTFILES/zsh/dircolors-solarized/dircolors.ansi-universal")"
fi

if (( $+commands[rbenv] )); then
  eval "$(rbenv init - zsh)"
fi

if (( $+commands[fasd] )); then
  eval "$(fasd --init auto)"
fi

if [[ -r "$HOME/.localrc" ]]; then
  source "$HOME/.localrc"
fi

# Initialize Starship last so the Zim steeef prompt cannot overwrite it.
if (( $+commands[starship] )); then
  if (( ! ${+_DOTFILES_STARSHIP_INITIALIZED} )); then
    eval "$(starship init zsh)"
    typeset -g _DOTFILES_STARSHIP_INITIALIZED=1
  fi
else
  unset _DOTFILES_STARSHIP_INITIALIZED
  if (( ! ${+_DOTFILES_ZIM_INITIALIZED} )); then
    PROMPT='%n@%m:%~%# '
  fi
fi

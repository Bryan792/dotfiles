[[ -o interactive ]] || return 0

export DOTFILES="${DOTFILES:-$HOME/.dotfiles}"

# Start configuration added by Zim install {{{
#
# User configuration sourced by interactive shells
#

# -----------------
# Zsh configuration
# -----------------

#
# History
#

# Remove older command from the history if a duplicate is to be added.
setopt HIST_IGNORE_ALL_DUPS

#
# Input/output
#

# Set editor default keymap to emacs (`-e`) or vi (`-v`)
bindkey -e

# Prompt for spelling correction of commands.
#setopt CORRECT

# Customize spelling correction prompt.
#SPROMPT='zsh: correct %F{red}%R%f to %F{green}%r%f [nyae]? '

# Remove path separator from WORDCHARS.
WORDCHARS=${WORDCHARS//[\/]}

# -----------------
# Zim configuration
# -----------------

# Use degit instead of git as the default tool to install and update modules.
#zstyle ':zim:zmodule' use 'degit'

# --------------------
# Module configuration
# --------------------

#
# git
#

# Set a custom prefix for the generated aliases. The default prefix is 'G'.
#zstyle ':zim:git' aliases-prefix 'g'

#
# input
#

# Append `../` to your input for each `.` you type after an initial `..`
#zstyle ':zim:input' double-dot-expand yes

#
# termtitle
#

# Set a custom terminal title format using prompt expansion escape sequences.
# See http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Simple-Prompt-Escapes
# If none is provided, the default '%n@%m: %~' is used.
#zstyle ':zim:termtitle' format '%1~'

#
# zsh-autosuggestions
#

# Disable automatic widget re-binding on each precmd. This can be set when
# zsh-users/zsh-autosuggestions is the last module in your ~/.zimrc.
ZSH_AUTOSUGGEST_MANUAL_REBIND=1

# Customize the style that the suggestions are shown with.
# See https://github.com/zsh-users/zsh-autosuggestions/blob/master/README.md#suggestion-highlight-style
#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=242'

#
# zsh-syntax-highlighting
#

# Set what highlighters will be used.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters.md
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)

# Customize the main highlighter styles.
# See https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md#how-to-tweak-it
#typeset -A ZSH_HIGHLIGHT_STYLES
#ZSH_HIGHLIGHT_STYLES[comment]='fg=242'

# ------------------
# Initialize modules
# ------------------

ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim
# Zim is installed by `make bootstrap-zim`; keep startup offline and safe when
# it has not been bootstrapped yet.
if (( ! ${+_DOTFILES_ZIM_INITIALIZED} )); then
  if [[ -r ${ZIM_HOME}/zimfw.zsh ]]; then
    # Install missing modules, and update ${ZIM_HOME}/init.zsh if missing or
    # outdated.  This is done once per shell so reload remains quiet.
    if [[ ! ${ZIM_HOME}/init.zsh -nt ${ZDOTDIR:-${HOME}}/.zimrc ]]; then
      source ${ZIM_HOME}/zimfw.zsh init -q
    fi
  fi
  # Initialize modules once.  Re-sourcing .zshrc must not register every Zim
  # completion and prompt hook a second time.
  if [[ -r ${ZIM_HOME}/init.zsh ]]; then
    source ${ZIM_HOME}/init.zsh
    typeset -g _DOTFILES_ZIM_INITIALIZED=1
  fi
fi

# ------------------------------
# Post-init module configuration
# ------------------------------

#
# zsh-history-substring-search
#

zmodload -F zsh/terminfo +p:terminfo
# Bind ^[[A/^[[B manually so up/down works both before and after zle-line-init
for key ('^[[A' '^P' ${terminfo[kcuu1]}) bindkey ${key} history-substring-search-up
for key ('^[[B' '^N' ${terminfo[kcud1]}) bindkey ${key} history-substring-search-down
for key ('k') bindkey -M vicmd ${key} history-substring-search-up
for key ('j') bindkey -M vicmd ${key} history-substring-search-down
unset key
# }}} End configuration added by Zim install

# shortcut to this dotfiles path is $ZSH
#export TERM=screen-256color
#export ZSH=$HOME/.oh-my-zsh
export CLASS=$HOME/Dropbox/cpe

if [ "$TERM" = "xterm" ]; then
export TERM='xterm-256color'
fi
# your project folder that we can `c [tab]` to
export PROJECTS=~/workspace

# Source legacy topic files once, in a deliberate order. This keeps the
# original aliases and functions without recursively loading prompt scripts.
_dotfiles_source() {
  [[ -r "$DOTFILES/$1" ]] && source "$DOTFILES/$1"
}

for config_file in \
  system/env.zsh \
  system/path.zsh \
  system/aliases.zsh \
  zsh/config.zsh \
  zsh/aliases.zsh \
  zsh/window.zsh \
  git/aliases.zsh \
  cas/aliases.zsh \
  ec2/aliases.zsh \
  jruby/aliases.zsh \
  nginx/aliases.zsh \
  postgresql/aliases.zsh \
  redis/aliases.zsh \
  todo.txt/aliases.zsh \
  vagrant/aliases.zsh \
  ruby/aliases.zsh \
  ruby/rbenv.zsh \
  android.zsh; do
  _dotfiles_source "$config_file"
done

# Keep aliases intact while completion determines the command context.  The
# migrated files are sourced through a helper function, so the legacy
# `setopt complete_aliases` in zsh/config.zsh is local to that helper.  Without
# restoring it here, `cd` expands to `_git_cd` before completion runs and Tab
# offers the function's arguments instead of directories.
setopt complete_aliases

# `gcd` uses the same Git-root-aware helper as `cd`; give it the native
# directory completer instead of falling back to unrestricted file matching.
if (( $+functions[compdef] )); then
  compdef _cd cd gcd
fi


# use .localrc for SUPER SECRET CRAP that you don't
# want in your public, versioned repo.
if [[ -a ~/.localrc ]]
then
  source ~/.localrc
fi

autoload -U zmv
autoload -U zsh-mime-setup
zsh-mime-setup

# Initialize completion if Zim was not bootstrapped.
if (( ! ${+_DOTFILES_ZIM_INITIALIZED} && ! ${+_DOTFILES_COMPINIT_INITIALIZED} )); then
  autoload -Uz compinit
  compinit -d "$HOME/.zcompdump"
  typeset -g _DOTFILES_COMPINIT_INITIALIZED=1
fi

# Load the legacy completion fragments after completion initialization.
for config_file in git/completion.sh zsh/completion.zsh ruby/completion.zsh; do
  _dotfiles_source "$config_file"
done

# start oh-my-zsh
# ZSH_THEME="mortalscumbag"
# plugins=(git)
# source $HOME/.zprezto/runcoms/zshrc
# System info at start of every terminal
# screenfetch

# Source Prezto.
#if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
#  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
#fi

if (( $+commands[fasd] )) ; then
  eval "$(fasd --init auto)"
fi
if (( $+commands[dircolors] )); then
  eval "$(dircolors --sh "$DOTFILES/zsh/dircolors-solarized/dircolors.ansi-universal")"
fi

alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS

ZSH_THEME_GIT_PROMPT_CACHE=1
[[ -d "$HOME/.npm/bin" ]] && path=("$HOME/.npm/bin" $path)
if (( $+commands[yarn] )); then
  yarn_bin="$(yarn global bin 2>/dev/null)"
  [[ -d "$yarn_bin" ]] && path=("$yarn_bin" $path)
fi
typeset -U path
export PATH

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

if (( $+commands[nvim] )); then
  alias vim="nvim"
  alias vi="nvim"
elif (( $+commands[vim] )); then
  alias vi="vim"
fi

# compinit

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Keep the legacy Zim steeef prompt as a fallback, but let Starship own the
# prompt whenever it is available.  The guard makes repeated `source
# ~/.zshrc` calls safe: Starship's hooks and ZLE wrapper are installed once.
if [[ ${TERM:-} != dumb ]]; then
  if (( $+commands[starship] )); then
    if (( ! ${+_DOTFILES_STARSHIP_INITIALIZED} )); then
      # steeef registers git-info as a precmd hook.  Remove that hook before
      # handing prompt rendering to Starship.
      if (( ${+functions[git-info]} )); then
	autoload -Uz add-zsh-hook
	add-zsh-hook -d precmd git-info 2>/dev/null || true
      fi
      eval "$(starship init zsh)"
      typeset -g _DOTFILES_STARSHIP_INITIALIZED=1
    fi
  else
    starship_was_active=0
    if (( ${+_DOTFILES_STARSHIP_INITIALIZED} )); then
      # Handle a Starship binary removed during a running shell as well as a
      # fresh shell without it; do not leave its hooks behind the fallback.
      autoload -Uz add-zsh-hook
      add-zsh-hook -d precmd prompt_starship_precmd 2>/dev/null || true
      add-zsh-hook -d preexec prompt_starship_preexec 2>/dev/null || true
      unset _DOTFILES_STARSHIP_INITIALIZED
      starship_was_active=1
    fi
    if (( ! ${+_DOTFILES_STEEEF_INITIALIZED} || starship_was_active )); then
      # Zim normally sourced this theme already.  Source it explicitly when
      # Zim is unavailable so the fallback remains usable on a fresh machine.
      if (( ! ${+_DOTFILES_ZIM_INITIALIZED} || starship_was_active )); then
	if [[ -r ${ZIM_HOME}/modules/steeef/steeef.zsh-theme ]]; then
	  source ${ZIM_HOME}/modules/steeef/steeef.zsh-theme
	else
	  PROMPT='%n@%m:%~%# '
	fi
      fi
      typeset -g _DOTFILES_STEEEF_INITIALIZED=1
    fi
    unset starship_was_active
  fi
fi

# A short alias for the safe, idempotent reload path; retain the legacy
# reload! spelling from zsh/aliases.zsh.
alias reload='source "$HOME/.zshrc"'

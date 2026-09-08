path=(
  "$HOME/bin"
  "./bin"
  "$HOME/.rbenv/shims"
  "$HOME/.rbenv/bin"
  /usr/local/bin
  /usr/local/sbin
  "$HOME/.sfs"
  "$DOTFILES/bin"
  $path
)
if [[ -d "$ANDROID_HOME/tools" ]]; then
  path+=("$ANDROID_HOME/tools")
fi
if [[ -d "/usr/local/texlive/2012/bin/x86_64-linux" ]]; then
  path+=("/usr/local/texlive/2012/bin/x86_64-linux")
fi
typeset -U path
export PATH

export MANPATH="/usr/local/man:/usr/local/mysql/man:/usr/local/git/man:$MANPATH"

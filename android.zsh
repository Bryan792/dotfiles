export ANDROID_HOME="${ANDROID_HOME:-/opt/android-sdk}"
if [[ -d "$ANDROID_HOME" ]]; then
  path=($path "$ANDROID_HOME/tools" "$ANDROID_HOME/platform-tools")
fi
if [[ -d "$HOME/Downloads/sbt/bin" ]]; then
  path=($path "$HOME/Downloads/sbt/bin")
fi
if [[ -d "$HOME/.config/bspwm/bin" ]]; then
  path=($path "$HOME/.config/bspwm/bin")
fi
if [[ -d "$HOME/Projects/Academic-Writing-Check" ]]; then
  path=($path "$HOME/Projects/Academic-Writing-Check")
fi
export JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/default/}"

if (( $+commands[ruby] )); then
  PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
fi

fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
    zle redisplay
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z
if [[ -d "$HOME/Downloads/lib/legacy/ubuntu-12.04" ]]; then
  export LD_LIBRARY_PATH="$HOME/Downloads/lib/legacy/ubuntu-12.04"
fi

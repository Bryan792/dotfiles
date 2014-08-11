export ANDROID_HOME=/home/bryan/Downloads/android/adt-bundle-linux-x86_64/adt-bundle-linux-x86_64/sdk
path=($path $ANDROID_HOME/tools $ANDROID_HOME/platform-tools /home/bryan/Downloads/sbt/bin /home/bryan/.config/bspwm/bin /home/bryan/Projects/Academic-Writing-Check) 
export JAVA_HOME=/usr/lib/jvm/java-7-openjdk

PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"

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
export LD_LIBRARY_PATH=/home/bryan/Downloads/lib/legacy/ubuntu-12.04/

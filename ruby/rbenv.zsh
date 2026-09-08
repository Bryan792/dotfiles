(( $+commands[rbenv] )) || return 0

# rehash shims
rbenv rehash 2>/dev/null

# shell thing
rbenv() {
  command="$1"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  shell)
    eval `rbenv "sh-$command" "$@"`;;
  *)
    command rbenv "$command" "$@";;
  esac
}

if (( $+commands[ruby] )); then
  PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
fi

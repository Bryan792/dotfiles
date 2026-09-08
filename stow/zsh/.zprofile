# Login-only setup is intentionally small; .zshenv owns the portable PATH.

if [[ -d "$HOME/.local/bin" ]]; then
  path=("$HOME/.local/bin" $path)
fi

# Legacy configuration migration

The `stow/` directory is the deployable source of truth.  Each package mirrors
the path it creates below `$HOME`; the installer always passes `--no-folding`
so shared directories such as `.config` and `.vim` remain real directories.

## Old-to-new mappings

| Legacy source | Stow destination | Package |
| --- | --- | --- |
| `bash/bash_profile.symlink` | `~/.bash_profile` | `bash` |
| `bash/bash_prompt.symlink` | `~/.bash_prompt` | `bash` |
| `bash/bashrc.symlink` | `~/.bashrc` | `bash` |
| `bash/inputrc.symlink` | `~/.inputrc` | `bash` |
| `zsh/zshenv.symlink` | `~/.zshenv` | `zsh` |
| `zsh/zprofile.symlink` | `~/.zprofile` | `zsh` |
| `zsh/zshrc.symlink` | `~/.zshrc` | `zsh` |
| `zsh/zimrc.symlink` | `~/.zimrc` | `zsh` |
| `zsh/zlogin.symlink` | `~/.zlogin` | `zsh` |
| `zsh/zlogout.symlink` | `~/.zlogout` | `zsh` |
| `git/gitconfig.symlink.example` | `~/.gitconfig` | `git` |
| `git/gitignore.symlink` | `~/.gitignore` | `git` |
| `git/gitattributes.symlink` | `~/.gitattributes` | `git` |
| `tmux/tmux.conf.symlink` | `~/.tmux.conf` | `tmux` |
| `vim/vimrc.symlink` | `~/.vimrc` | `vim` |
| `vim/gvimrc.symlink` | `~/.gvimrc` | `vim` |
| `vim/vim.symlink/` | `~/.vim/` | `vim` |
| `ruby/gemrc.symlink` | `~/.gemrc` | `ruby` |
| `ruby/irbrc.symlink` | `~/.irbrc` | `ruby` |
| `wget/wgetrc.symlink` | `~/.wgetrc` | `wget` |
| `irssi/irssi.symlink/` | `~/.irssi/` | `irssi` |
| `xmonad/xmonad.symlink/` | `~/.xmonad/` | `xmonad` |
| (new) | `~/.config/starship.toml` | `starship` |
| (new) | `~/.config/nvim/init.vim` | `neovim` |

The legacy topic files under `system/`, `zsh/`, `git/`, and the application
directories remain available to `.zshrc` through deliberate, ordered loading.
The old recursive Zsh loader and duplicate prompt entrypoints are archived so
they cannot run twice.  The divergent top-level `.xmonad` tree, generated
artifacts, self-referential links, and dormant Prezto content are under
`archive/legacy-generated/` and `archive/legacy-prezto/`.

The `git` and `vim` packages contain a package-local Stow ignore list.  It
retains Stow's safety defaults while allowing the real `.gitignore` files to be
linked; `bin/stow-preflight` ignores the package control file itself.

## Package selection and dependencies

Gura's active profile is `zsh starship git tmux vim neovim bash wget`:

```sh
make dry-run
make install BACKUP=1
make bootstrap-zim
make bootstrap-starship
make bootstrap-git-identity
```

The default profile needs GNU Stow, GNU Make, Zsh, tmux, curl, Git, and the
Starship binary.  `bootstrap-zim` downloads Zim and builds its module init;
`bootstrap-starship` installs the ARM64-compatible Starship binary into
`~/.local/bin`; the identity include is untracked and mode `600`.

The active profile links the Bash, Vim, Neovim, and Wget configuration files;
it does not install their runtimes or Vim plugins. The remaining packages are
selected explicitly:

* `ruby`, `irssi`, and `xmonad` need their corresponding programs and are
  retained for opt-in desktop or workstation use.

## Safe migration and rollback

Run `make dry-run PACKAGES='...' TARGET=/path/to/test-home` before an isolated
install.  Correct links are no-ops.  Unrelated files and parent-directory
links are reported without descending through or backing up their children.
`make install BACKUP=1` moves each conflicting target to a unique path under
`~/.dotfiles-backups/<UTC timestamp>/` and then invokes Stow.  The installer
never uses `--adopt`.

To remove a package, use the same package list with `make uninstall`; Stow
removes only links owned by that package and leaves unrelated files alone.
Keep an SSH session open while changing a login shell.  If a rollback is
needed, uninstall the selected packages, restore files from the recorded
backup directory, and use `sudo chsh -s /bin/sh bryan` only if the shell itself
must be reverted.

Momo is not part of this rollout.  A future Momo migration should use a fresh
checkout, inspect its existing links with `make dry-run`, choose a separate
backup directory, and deploy only after reviewing conflicts.  Do not point the
Gura checkout at Momo or run `--adopt` there.

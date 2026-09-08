# bryan792 : dotfiles

## Bryan792 Intro
My setup of dotfiles forked from forks of forks.

## Intro
These are the dotfiles I am using (on Linux!). Expect modifications to continue
to these files. These dotfile projects (in general) are pieced together
from various other dotfile projects. A comprehensive list of who did what
could be difficult. So here is my lazy stab at it:
- [Michael Bleigh's](https://github.com/mbleigh) dotfiles is what my dotfiles originated from
- [Zach Holman's](https://github.com/holman) Michael forked from
- [Mathias Bynens](https://github.com/mathiasbynens) had some neat stuff I've included here

## install

- `git clone https://github.com/Bryan792/dotfiles.git ~/.dotfiles`
- `cd ~/.dotfiles && git switch -c stow-starship-gura`
- `sudo apt-get update && sudo apt-get install -y make stow zsh tmux curl`
- `make dry-run`
- `make install`
- `make bootstrap-zim`
- `make bootstrap-starship`
- `make bootstrap-git-identity`

GNU Stow links the default terminal profile (`zsh`, `starship`, `git`, and
`tmux`) from `stow/` into the home directory. Stow is always run with
`--no-folding`, so shared directories such as `.config` remain real
directories. Neovim is available as an opt-in package but is not installed by
the default profile.

The complete legacy mapping is in
[`docs/legacy-stow-migration.md`](docs/legacy-stow-migration.md). It records
the original paths, optional package dependencies, archived generated files,
and the safe Momo migration procedure. The original Vim configuration and
support tree are preserved under `stow/vim/`; plugin installation remains an
explicit later step.

Use `make dry-run` before changing a target. Existing correct links are a
no-op; unrelated files and links are reported as conflicts. To preserve a
conflict under a unique backup name and continue, use `make install BACKUP=1`.
`make restow` reapplies the selected packages, and `make uninstall` removes
only links managed by those packages.

## modify and reload

- `reload` will load changes to the active Zsh configuration. The legacy
  `reload!` alias remains available.

## topical

Legacy topic files remain in the repository and are loaded once in a deliberate
order by the migrated `.zshrc`. The active profile uses only the explicit
packages under `stow/`; desktop and application configuration is not activated
on Gura.

## what's inside

A lot of stuff. Seriously, a lot of stuff. Check them out in the file browser
above and see what components may mesh up with you. Fork it, remove what you
don't use, and build on what you do use.

## components

There's a few special files in the hierarchy.

- **bin/**: Anything in `bin/` will get added to your `$PATH` and be made
  available everywhere.
- **stow/**: Explicit packages whose paths mirror destinations under `$HOME`.
- **stow/neovim/**: A deferred Neovim entrypoint; deploy it later with
  `make install PACKAGES=neovim` after installing Neovim and its runtimes.

Momo should migrate its old links separately with the same dry-run and backup
process. This checkout does not modify Momo.

## bugs

If you run into any problem, just submit a bug and I'll look into it. These 
dotfiles are only intended to work on a Linux distro. Just [open an issue](https://github.com/dsnyder/dotfiles/issues) on this repository
if you hit anything nasty.

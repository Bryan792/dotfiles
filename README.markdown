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

The deployable configuration lives in `stow/`. Install from a checkout that
contains the `stow-starship-gura` rollout branch. A fresh clone of `master` may
still contain only the legacy installer; once the rollout branch is published,
select it explicitly:

```sh
git clone https://github.com/Bryan792/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
git fetch origin stow-starship-gura
git switch --track -c stow-starship-gura origin/stow-starship-gura
```

If the branch was supplied locally, use `git switch stow-starship-gura`.
Creating an empty branch with `git switch -c stow-starship-gura` does not fetch
the rollout commits.

Install the required tools, preview the links, and bootstrap the profile:

```sh
sudo apt-get update
sudo apt-get install -y git make stow zsh tmux curl

make dry-run
make bootstrap BACKUP=1
```

`make bootstrap` installs the Stow links, initializes Zim, installs Starship
under `~/.local/bin`, and creates the untracked Git identity include for Bryan
Ching (`bryan792@gmail.com`). `BACKUP=1` preserves conflicting files under a
timestamped directory in `~/.dotfiles-backups/`. The default package list is:

```text
zsh starship git tmux vim neovim bash wget
```

The Vim and Neovim configuration files are linked, but Neovim, Vim plugins,
language runtimes, and fonts are not installed by this setup. Install those
later when needed. Stow uses `--no-folding`, so shared directories such as
`.config` remain real directories.

Optional packages can be previewed and installed separately:

```sh
make dry-run PACKAGES='ruby irssi xmonad'
make install BACKUP=1 PACKAGES='ruby irssi xmonad'
```

After installation, start a fresh shell and validate the terminal profile
before changing the login shell. Keep the current SSH session open while
testing and verify a second SSH session from the client:

```sh
exec zsh
chsh -s "$(command -v zsh)"
```

Run the `chsh` command only after Zsh, completions, Starship, Git, and tmux
work as expected. To roll back managed links, use the same package selection
with `make uninstall`; restore any files from the recorded backup directory if
needed. The login shell can be restored with `sudo chsh -s /bin/sh bryan`.

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
- **stow/neovim/**: The Neovim entrypoint, linked with the default profile. It
  can be used after Neovim and its plugins are installed.

Momo should migrate its old links separately with the same dry-run and backup
process. This checkout does not modify Momo.

## bugs

If you run into any problem, just submit a bug and I'll look into it. These 
dotfiles are only intended to work on a Linux distro. Just [open an issue](https://github.com/dsnyder/dotfiles/issues) on this repository
if you hit anything nasty.

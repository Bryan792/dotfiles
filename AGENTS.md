# Repository instructions

## Scope

This checkout contains Bryan's dotfiles and the Gura terminal rollout. Work on
the `stow-starship-gura` branch unless the user directs otherwise. The rollout
keeps Gura on Ubuntu 24.04; do not attempt the deferred Ubuntu upgrade, replace
the OCI instance, or alter its unrelated services, SSH configuration, or
networking.

Momo is a separate machine with existing local changes. Do not access, deploy
to, or modify Momo as part of work in this repository. Document any future
Momo migration instead of performing it here.

## Configuration source and deployment

- `stow/` is the deployable source of truth. Package paths mirror their final
  locations below `$HOME`.
- The default profile is `zsh starship git tmux vim neovim bash wget`.
- `ruby`, `irssi`, and `xmonad` are opt-in packages; do not activate them on
  Gura unless the user asks.
- Use the Makefile interface and its `--no-folding` Stow mode. Never use
  `stow --adopt` and never silently overwrite a conflict.
- Run `make dry-run` before deployment. Use `BACKUP=1` for a first install or
  an approved conflict migration; backups belong under the timestamped
  `~/.dotfiles-backups/` directory.
- `make uninstall` may remove only links managed by the selected packages.
  Preserve unrelated files and links.

The canonical mapping and migration rules are in
[`docs/legacy-stow-migration.md`](docs/legacy-stow-migration.md). The Gura
deployment record, installed versions, backup locations, rollback steps, and
known deferred checks are in [`docs/gura-terminal.md`](docs/gura-terminal.md).

## Change policy

Preserve the legacy configuration as the source of truth. Keep aliases,
functions, options, keybindings, comments, and plugin declarations unless a
change is required for a relocated path, portable startup, or the approved
Starship integration. Keep `.vimrc` and other editor files as close to their
legacy contents as possible. Do not add Neovim, Vim plugins, language
runtimes, fonts, or other software unless the user explicitly requests it.

Zim remains the Zsh plugin and completion manager. Starship is the active
Tokyo Night prompt, with the legacy steeef prompt available as a fallback when
Starship is unavailable. Keep startup loading deliberate and repeatable: avoid
duplicate sourcing, guard optional tools, and preserve directory completion
for both `cd` and its Git-aware aliases.

Do not initialize or restore Prezto. Keep generated artifacts, self-referential
links, and divergent legacy trees archived rather than reproducing them in
active packages. Do not add another author's Git identity; Bryan's identity
include is generated locally and remains untracked.

## Validation and commits

For repository-only changes, test Stow in a temporary target before touching a
real home directory. At minimum, check dry-run, install, restow, conflict
backup, uninstall, broken links, fresh and repeated Zsh startup, completion,
Starship fallback, Git configuration, and tmux parsing when those areas are
affected. Editor layout may be checked without installing editors or plugins.

Keep changes focused on the user's request, update the relevant documentation,
and commit only related files on the rollout branch. Do not push or merge
without explicit direction. Before changing a login shell on Gura, keep an SSH
session open and verify a second session from the client.

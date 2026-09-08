# Gura terminal profile

This record describes the Gura rollout. It is deliberately separate from the
legacy desktop and application configuration retained in this repository.

## Bootstrap

From a fresh checkout at `/home/bryan/.dotfiles`:

```sh
sudo apt-get update
sudo apt-get install -y make stow zsh tmux curl
make dry-run
make install BACKUP=1
make bootstrap-zim
make bootstrap-starship
make bootstrap-git-identity
```

The default packages are `zsh starship git tmux vim neovim bash wget`. The
complete legacy mapping and remaining optional package dependencies are in
[`legacy-stow-migration.md`](legacy-stow-migration.md). Use
`TARGET=/path/to/test-home make dry-run` and the corresponding `install`,
`restow`, and `uninstall` commands exercise a temporary target. The first
install on a stock Ubuntu account may need `BACKUP=1` to preserve its existing
`.bashrc`.

`make install` runs `bin/stow-preflight` before Stow. Correct existing links
are reported as no-ops. Conflicting files and unrelated links stop the
operation; `make install BACKUP=1` moves each conflict to a unique file under
`~/.dotfiles-backups/<UTC timestamp>/` before retrying. `make uninstall` uses
Stow's delete mode and removes managed links only.

## Gura record

- Host: `gura`, user `bryan`, ARM64 Oracle Cloud instance.
- OS retained: Ubuntu 24.04.3 LTS (the 26.04 upgrade was deferred).
- Kernel observed during rollout: `6.17.0-1011-oracle`; a newer kernel is
  pending reboot.
- Installed versions: GNU Stow 2.3.1, GNU Make 4.3, Zsh 5.9, tmux 3.4,
  Starship 1.26.0.
- Git identity include: `~/.config/git/identity.inc` (mode 600), containing
  Bryan Ching and `bryan792@gmail.com`; it is intentionally untracked.
- Starship config: `~/.config/starship.toml`, the unchanged standard Tokyo
  Night preset linked from `stow/starship/`.
- No host fonts, Neovim binary, editor plugins, or language runtimes were
  installed. The Vim, Neovim, Bash, and Wget configuration packages are
  linked.
- Remaining client-side check: confirm Tokyo Night colors and Nerd Font glyphs
  in Termius or another Nerd-Font-capable terminal.
- Terminal-profile rollback backup: `/home/bryan/.dotfiles-backups/pre-migration-20260908T023541Z/`.
- The existing Ubuntu `.bashrc` was preserved at
  `/home/bryan/.dotfiles-backups/20260908T031439Z/home__bryan__.bashrc`.
  Future backups use the timestamped directory described above.
- Login shell after validation: `/usr/bin/zsh`.
- `sshd -t`, the SSH service, and existing Docker, Tailscale, Periphery,
  containerd, and OCI agent services remained healthy. A second authenticated
  SSH session could not be opened from this agent because no local private key
  matches Gura's existing authorized keys; validate that from the client
  before closing the original session.

The repository branch is `stow-starship-gura`. The migration commit is the
commit recorded by `git rev-parse HEAD` after the final validation; no commit
is pushed or merged by this rollout.

Validation covered a full optional-package dry-run and isolated install,
restow, conflict backup, parent-link handling, uninstall, broken-link checks,
fresh and repeated Zsh startup, missing-Starship and `TERM=dumb` fallbacks,
Git and non-Git Starship prompts, Git identity, tmux parsing, SSH syntax, and
the running SSH, Docker, Tailscale, Periphery, containerd, OCI agent, and
monitoring services. Editor layout and Neovim entrypoints were checked as
files only; editor, plugin, and runtime execution remains deferred.

## Rollback

1. Keep an existing SSH session open while changing the login shell.
2. Run `make uninstall` for the same package set; this removes only managed
   links. Restore any recorded files from the timestamped backup directory.
3. Restore the login shell with `sudo chsh -s /bin/sh bryan` if needed.
4. Leave the Ubuntu upgrade deferred; OS recovery requires the verified OCI
   boot-volume and console procedure and is not an apt downgrade.

Momo migration is a separate operation. On Momo, inspect and back up each old
link with `make dry-run` before deploying an equivalent package set; do not
run this checkout's commands against Momo as part of the Gura rollout.

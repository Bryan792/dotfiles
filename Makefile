SHELL := /bin/sh

DOTFILES := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
STOW_DIR := $(DOTFILES)/stow
TARGET ?= $(HOME)
PACKAGES ?= zsh starship git tmux vim neovim bash wget
BACKUP_DIR ?= $(HOME)/.dotfiles-backups/$(shell date -u +%Y%m%dT%H%M%SZ)
STOW_FLAGS := --dir="$(STOW_DIR)" --target="$(TARGET)" --no-folding
PREFLIGHT := $(DOTFILES)/bin/stow-preflight

.DEFAULT_GOAL := help

.PHONY: help dry-run install restow uninstall bootstrap-zim bootstrap-starship bootstrap-git-identity bootstrap install-deps

help:
	@printf '%s\n' \
		'make dry-run                         Preview managed links' \
		'make install                         Install the terminal profile' \
		'make install BACKUP=1               Back up conflicts, then install' \
		'make restow                         Reapply managed links' \
		'make uninstall                      Remove managed links only' \
		'make bootstrap-zim                  Install Zim and generate its init' \
		'make bootstrap-starship             Install Starship and restow its preset' \
		'make bootstrap-git-identity         Create the untracked Git identity include' \
		'make bootstrap                     Install links, Zim, and Starship' \
		'PACKAGES="..." TARGET="..."       Override packages or target for tests'

dry-run:
	@$(PREFLIGHT) --stow-dir "$(STOW_DIR)" --target "$(TARGET)" $(PACKAGES)
	@stow $(STOW_FLAGS) --simulate --verbose=1 $(PACKAGES)

install:
	@$(PREFLIGHT) --stow-dir "$(STOW_DIR)" --target "$(TARGET)" \
		$(if $(filter 1 yes true,$(BACKUP)),--backup-dir "$(BACKUP_DIR)",) $(PACKAGES)
	@stow $(STOW_FLAGS) $(PACKAGES)

restow:
	@$(PREFLIGHT) --stow-dir "$(STOW_DIR)" --target "$(TARGET)" \
		$(if $(filter 1 yes true,$(BACKUP)),--backup-dir "$(BACKUP_DIR)",) $(PACKAGES)
	@stow $(STOW_FLAGS) --restow $(PACKAGES)

uninstall:
	@stow $(STOW_FLAGS) --delete $(PACKAGES)

install-deps:
	sudo apt-get update
	sudo apt-get install -y make stow zsh tmux curl

bootstrap-zim: install
	@command -v curl >/dev/null || { echo 'curl is required to bootstrap Zim' >&2; exit 1; }
	@mkdir -p "$(TARGET)/.zim"
	@curl -fsSL --create-dirs -o "$(TARGET)/.zim/zimfw.zsh" \
		https://github.com/zimfw/zimfw/releases/latest/download/zimfw.zsh
	@ZDOTDIR="$(TARGET)" HOME="$(TARGET)" ZIM_HOME="$(TARGET)/.zim" zsh -fc \
		'source "$$HOME/.zim/zimfw.zsh" init -q'
	@echo 'Zim modules initialized under $(TARGET)/.zim'

bootstrap-starship: install
	@mkdir -p "$(TARGET)/.local/bin"
	@curl -sS https://starship.rs/install.sh | sh -s -- \
		--yes --bin-dir "$(TARGET)/.local/bin"
	@$(MAKE) --no-print-directory restow PACKAGES=starship TARGET="$(TARGET)"

bootstrap-git-identity: install
	@mkdir -p "$(TARGET)/.config/git"
	@if [ -e "$(TARGET)/.config/git/identity.inc" ]; then \
		echo "Preserving existing $(TARGET)/.config/git/identity.inc"; \
	else \
		git config --file "$(TARGET)/.config/git/identity.inc" user.name "Bryan Ching"; \
		git config --file "$(TARGET)/.config/git/identity.inc" user.email "bryan792@gmail.com"; \
		chmod 600 "$(TARGET)/.config/git/identity.inc"; \
		echo "Created $(TARGET)/.config/git/identity.inc"; \
	fi

bootstrap: install bootstrap-zim bootstrap-starship bootstrap-git-identity

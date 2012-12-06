# bryan792 : dotfiles

## Intro
These are the dotfiles I am using (on Linux!). Expect modifications to continue
to these files. These dotfile projects (in general) are pieced together
from various other dotfile projects. A comprehensive list of who did what
could be difficult. So here is my lazy stab at it:
- [Michael Bleigh's](https://github.com/mbleigh) dotfiles is what my dotfiles originated from
- [Zach Holman's](https://github.com/holman) Michael forked from
- [Mathias Bynens](https://github.com/mathiasbynens) had some neat stuff I've included here

## install

- `git clone git://github.com/bryan792/dotfiles ~/.dotfiles`
- `cd ~/.dotfiles`
- `rake install`

The install rake task will symlink the appropriate files in `.dotfiles` to your
home directory. Additionally, it will clone [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) into your home directory as `.oh-my-zsh`
**WARNING: Currently the cloning just nukes your current ~/.oh-my-zsh if it exists**

Things you will want to take a glance at and tweak:
- zsh/zshrc.symlink
- system/\*
- git/gitconfig.symlink.example

## modify and reload

- `reload!` will load any changes you have made to any of the files 

## topical

Everything's built around topic areas. If you're adding a new area to your
forked dotfiles — say, "Java" — you can simply add a `java` directory and put
files in there. Anything with an extension of `.zsh` will get automatically
included into your shell. Anything with an extension of `.symlink` will get
symlinked without extension into `$HOME` when you run `rake install`.

## what's inside

A lot of stuff. Seriously, a lot of stuff. Check them out in the file browser
above and see what components may mesh up with you. Fork it, remove what you
don't use, and build on what you do use.

## components

There's a few special files in the hierarchy.

- **bin/**: Anything in `bin/` will get added to your `$PATH` and be made
  available everywhere.
- **topic/\*.zsh**: Any files ending in `.zsh` get loaded into your
  environment.
- **topic/\*.symlink**: Any files ending in `*.symlink` get symlinked into
  your `$HOME`. This is so you can keep all of those versioned in your dotfiles
  but still keep those autoloaded files in your home directory. These get
  symlinked in when you run `rake install`.
- **topic/\*.completion.sh**: Any files ending in `completion.sh` get loaded
  last so that they get loaded after we set up zsh autocomplete functions.

## bugs

If you run into any problem, just submit a bug and I'll look into it. These 
dotfiles are only intended to work on a Linux distro. Just [open an issue](https://github.com/dsnyder/dotfiles/issues) on this repository
if you hit anything nasty.

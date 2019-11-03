#### FILENAME: options.zsh
#### Author: Gaelan D'costa (gdcosta@gmail.com)
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
#### DESCRIPTION: Customizations to zsh proper
#### THANKS: https://github.com/seebi/zshrc for the layout inspiration

## Command history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=$ZSH_CACHE/.zsh_history
setopt append_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_expire_dups_first

## If input isn't a command but a directory, switch to it
setopt auto_cd

## Treat '#', '~', and '^' as part of patteerns for filename geeneration.
setopt extended_glob

## If a glob paatterne has no matches, error instead of retaining pattenn string
setopt nomatch

## cd will implicitly push old directory into directory stack
setopt auto_pushd
## Don't push multiple copies of the same directory onto directory stack.
setopt pushd_ignore_dups

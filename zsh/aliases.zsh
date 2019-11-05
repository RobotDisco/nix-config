#### FILENAME: aliases.zsh
#### Author: Gaelan D'costa (gdcosta@gmail.com)
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
#### DESCRIPTION: User aliases
#### THANKS: https://github.com/seebi/zshrc for the layout inspiration

### Emacs is my editor
export EDITOR="emacsclient -t --alternate-editor=''"
export VISUAL="emacsclient -nc --alternate-editor=''"
export GIT_EDITOR="emacsclient -c --alternate-editor=''"
alias e="$VISUAL"

## so use its keybindings in the zsh line editor
bindkey -e

## If vim doesn't exist, invoke vi instead
command -v vim --help >/dev/null 2>&1 || alias vim=vi

## Sometimes emacsclient gets stuck, so we need to kill the daemon
alias killemacs="emacsclient -e '(kill-emacs)'"

### edit common files
alias aedit=" $EDITOR $ZSH_CONFIG/aliases.zsh; source $ZSH_CONFIG/aliases.zsh"
alias fedit=" $EDITOR $ZSH_CONFIG/functions.zsh; source $ZSH_CONFIG/functions.zsh"
alias pedit=" $EDITOR $ZSH_CONFIG/private.zsh; source $ZSH_CONFIG/private.zsh"
alias eedit=" $EDITOR $HOME/.emacs.d/config.org"

### Optimize standard tools
## colourize grep
#alias grep='grep --color=auto'

## make ls look nicer
#alias ls=' ls --color=auto -Fph'
alias pgrep='pgrep -a'







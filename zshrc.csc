# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=2000
SAVEHIST=2000
setopt appendhistory autocd nomatch extendedglob
unsetopt beep notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/gaelan/.zshrc'

autoload -Uz compinit
compinit
#end of lines added by compinstall

# Add user-supplied programs to our environment
if [[ -d $HOME/bin ]] ; then
  typeset -U path
  path+=$HOME/bin
fi
if [[ -d $HOME/sr/coroner ]] ; then
  typeset -U path
  path+=$HOME/sr/coroner
fi
if [[ -d $HOME/man  ]] ; then
  typeset -U manpath
  manpath+=$HOME/man
fi

## Mundane aliases ##
alias ls='ls --color=auto -Fh'
alias grep='grep --color'

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

# Perforce setup
export P4PORT=p4proxy.waterloo.bluecoat.com:1666
export P4USER=gaelan.dcosta
export P4DIFF=kdiff3
export P4MERGE=kdiff3

## Universal Truths about our Bluecoat Dev environment ##
export QUIETBLD=1
export CL_OBJTYPE=release_ca
export PRODUCTS=cf_2xx.bind

# Almost a universal truth -- platform dependant
if [[ $(uname -o) == "Cygwin" ]] ; then
  export PERL_HOME=c:/Perl
else
  export PERL_HOME=/usr
fi

## Workflow aliases and functions ##
cgrep () {
	find . -iname '*.[ch]' -or -iname '*.[ch]pp' -print0 | xargs -0 grep -n $1
}
hgrep () { 
	find . -iname '*.h' -or -iname '*.hpp' -print0 | xargs -0 grep -n $1
}
jgrep () {
	find . -iname '*.java' -print0 | xargs -0 grep -n $1
}

alias sustain_rdp='rdesktop -u gaelan.dcosta -g 1024x768 -d CF-CAL -p - sustain-1.waterloo.bluecoat.com'
alias 2xx_install='cp wdir/2xx.chk_dbg /var/www/2xx.img'
alias 2xx_rinstall='cp wdir/2xx.chk_rls /var/www/2xx.img'

## Mundane aliases ##
alias ls='ls --color=auto -Fh'
alias grep='grep --color'

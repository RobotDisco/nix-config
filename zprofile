# Add user-supplied programs to our environment
if [[ -d $HOME/bin ]]; then
  typeset -U path
  path+=$HOME/bin
fi
if [[ -d $HOME/.cabal/bin ]]; then
  typeset -U path
  path+=$HOME/.cabal/bin
fi
if [[ -d $HOME/slickedit/bin ]]; then
  typeset -U path
  path+=$HOME/slickedit/bin
fi
if [[ -d $HOME/proj/scorpius/main/scripts ]]; then
  typeset -U path
  path+=$HOME/proj/scorpius/main/scripts
fi
if [[ -d $HOME/sr/coroner ]]; then
  typeset -U path
  path+=$HOME/sr/coroner
fi
if [[ -d $HOME/man  ]]; then
  typeset -U manpath
  manpath+=$HOME/man
fi

# Just in case we have real 'vi', prefer sane 'vim'
export EDITOR=vim

# Perforce setup
export P4PORT=p4proxy.waterloo.bluecoat.com:1666
export P4USER=gaelan.dcosta
export P4DIFF=kdiff3
export P4MERGE=p4kdiff3merge

# Universal Truths about our Bluecoat Dev environment ## (pre-Scorpius)
export QUIETBLD=1
export CL_CODEVIEW=1
export LINK_UTESTS=1
export CL_OBJTYPE="release_ca"
export PRODUCTS="cf_210.bind"

# Universal Truths ... (Scorpius)
export BLD_DEBUG=1
export BLD_QUIET=1
export BLD_TRACE="all"
export BLD_LISTING_FILES=1
export KNL_ENABLE_DEBUG_ASSERTIONS=1

# Almost a universal truth -- platform dependant
if [[ $(uname -o) == "Cygwin" ]] ; then
  export PERL_HOME=c:/Perl
  #export TERM=xterm-color # Cygwin is hobo re: urxvt
else
	export PERL_HOME=/usr
fi

if [[ $(uname -o) == "Linux" || $(uname -o) == "GNU/Linux" ]] ; then
	export BUILD_SKIP_DIRS="loader dos_boot"
fi

# load profiles from /etc/profile.d
#   (to disable a profile, just remove execute permission on it)
for profile in /etc/profile.d/*.sh; do
  if [[ -x $profile ]]; then
    . $profile
  fi
done
unset profile


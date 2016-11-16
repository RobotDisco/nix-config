# Print a friend message on exit
function on_exit --on-process %self
    if status --is-login
	echo "Thank you. Come again!"
	echo "  -- Dr. Apu Nahasapeemapetilon"
    end
end

# If fortune exists and this is an interactive shell, print a pithy quote
function fish_greeting
    if type -f "fortune" > /dev/null ^&1
	fortune -s
	echo
    end
end

# Emacs cask support and some nice aliases
if type -f "cask" >/dev/null ^/dev/null
    set -x PATH $PATH "$HOME/.cask/bin"

    alias ca='cask'
    alias cai='cask install'
    alias cau='cask update'
    alias caI='cask init'
    alias cae='cask exec'
end

# Homebrew
if test (uname) = "Darwin"
    # Standard homebrew
    alias brewc='brew cleanup'
    alias brewC='brew cleanup --force'
    alias brewi='brew install'
    alias brews='brew search'
    alias brewu='brew upgrade'
    alias brewU='brew update ;and brew upgrade'
    alias brewx='brew remove'

    # Homebrew Cask
    alias bcask='brew cask'
    alias caskc='brew cask cleanup --outdated'
    alias caskC='brew cask cleanup'
    alias caski='brew cask install'
    alias caskl='brew cask list'
    alias casks='brew cask search'
    alias caskx='brew cask uninstall'
end

# Emacs
set -x EDITOR "emacsclient"

# RBEnv support
status --is-interactive; and . (rbenv init -|psub)

# Pyenv support
status --is-interactive; and . (pyenv init -|psub)

# FASD support
function init --on-event init_fasd
  if not available fasd
    echo "🍒  Please install 'fasd' first!"
  else
    function -e fish_preexec _run_fasd
      fasd --proc (fasd --sanitize "$argv") > "/dev/null" 2>&1
    end

    function j
      cd (fasd -d -e 'printf %s' "$argv")
    end
  end
end

# ops/arc support from work
set PATH $PATH ~/workspace/dev_scripts/arcanist/bin ~/workspace/ops/bin ~/bin

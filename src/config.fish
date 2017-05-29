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
set -x EDITOR "emacsclient -a vim"
set -x VISUAL $EDITOR

# RBEnv support
status --is-interactive; and . (rbenv init -|psub)

# Pyenv support
status --is-interactive; and . (pyenv init -|psub)

# FASD support
if type -q fasd
    function __run_fasd -e fish_preexec
      command fasd --proc (command fasd --sanitize "$argv" | tr ' ' \n) > "/dev/null" 2>&1
    end

    function j
      cd (fasd -d -e 'printf %s' "$argv")
    end
else
    echo "🍒  Please install 'fasd' first!"
end

# ops/arc support from work
set -x PATH $PATH ~/workspace/dev_scripts/arcanist/bin ~/bin

function old_ssh
    command ssh -F ~/.ssh/config.old "$argv"
end

function new_ssh
    command ssh -F ~/.ssh/config.new "$argv"
end

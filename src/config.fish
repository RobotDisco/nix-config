# Print a friend message on exit
function on_exit --on-process %self
    if status --is-login
        echo "Thank you. Come again!"
        echo "  -- Dr. Apu Nahasapeemapetilon"
    end
end

# If fortune exists and this is an interactive shell, print a pithy quote
function fish_greeting
    if type -f "fortune" > /dev/null
        fortune -s
        echo
    end
end

# Emacs cask support and some nice aliases
if test -d "$HOME/.cask"
    set PATH $PATH "$HOME/.cask/bin"

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
    alias brewU='brew update and brew upgrade'
    alias brewx='brew remove'

    # Homebrew Cask
    alias cask='brew cask'
    alias caskc='brew cask cleanup --outdated'
    alias caskC='brew cask cleanup'
    alias caski='brew cask install'
    alias caskl='brew cask list'
    alias casks='brew cask search'
    alias caskx='brew cask uninstall'
end

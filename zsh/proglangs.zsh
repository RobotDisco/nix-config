#### FILENAME: proglangs.zsh
#### Author: Gaelan D'costa (gdcosta@gmail.com)
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
#### DESCRIPTION: Sjpport for various programming language environments

### Ruby
command -v rbenv >/dev/null 2>&1 && eval "$(rbenv init -)"

### Python
command -v pyenv >/dev/null 2>&1 && eval "$(pyenv init -)"

### Javascript (Node / NPM)
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

### Haskell (GHCup)
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

### Golang
export GOPATH=$HOME/code/go
export PATH=$PATH:$GOPATH/bin

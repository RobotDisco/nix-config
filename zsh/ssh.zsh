#### FILENAME: ssh.zsh
#### Author: Gaelan D'costa (gdcosta@gmail.com)
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
#### DESCRIPTION: SSH helpers (in particular, ssh key management)

## An implicit way of saying this is my work machine (i.e. only work key)
if [[ `uname` == 'Darwin' ]]; then
    SSH_KEYS="id_rsa"
else
    ## An impliit way of saying this is my home machine (work/personal keys)
    SSH_KEYS="id_rsa id_rsa.work"
fi

command -v keychain >/dev/null 2>&1 && eval "$(keychain --quiet --agents ssh --eval $=SSH_KEYS)"

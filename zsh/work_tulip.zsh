#### FILENAME: work_tulip.zsh
#### Author: Gaelan D'costa (gdcosta@gmail.com)
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
#### DESCRIPTION: Work-specific aliases and helpers
#### THANKS: https://github.com/seebi/zshrc for the layout inspiration


## Include tdocker dir into PATH
if [[ -d ~/workspace/dev_scripts/docker/bin ]]; then
   export PATH=$PATH:$HOME/workspace/dev_scripts/docker/bin
fi

### Tulip workflow
function tclone () {
        mkdir -p ~/workspace/$1 && git clone git@git.internal.tulip.io:$1.git ~/workspace/$1
}
alias tdl=" tdocker login"

#### FILENAME: fasd.zsh
#### AUTHOR: Gaelan D'costa <gdcosta@gmail.com>
#### LICENSE: https://opensource.org/licenses/GPL-3.0
####
####+BEGIN DESCRIPTION
#### fasd integration (filesystem fuzzy matching)
#### https://github.com/clvv/fasd
####+END DESCRIPTION

### initialize fasd, configures completion, hooks, aliases
command -v fasd >/dev/null 2>&1 && eval "$(fasd --init auto)"

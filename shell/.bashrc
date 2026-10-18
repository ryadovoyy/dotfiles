# if not running interactively, don't do anything
[ -z "$PS1" ] && return

# enable infinite history
HISTSIZE=
HISTFILESIZE=

# show date and time in history
HISTTIMEFORMAT='%Y-%m-%d %T '

# don't put duplicate lines or lines starting with space in history
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# update LINES and COLUMNS values automatically
shopt -s checkwinsize

# enable vi keybindings
set -o vi

# set prompt
PS1="\[\033[01;32m\]\u\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\[\033[01;32m\]\$(git_branch)\[\033[00m\] λ "

git_branch() {
  git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# set terminfo
TERM=xterm-16color
COLORTERM=

# add aliases
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"

# add fzf support
eval "$(fzf --bash)"

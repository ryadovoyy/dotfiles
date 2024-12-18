# if not running interactively, don't do anything
[ -z "$PS1" ] && return

# enable infinite history
HISTSIZE=
HISTFILESIZE=

# show date and time in the history
HISTTIMEFORMAT='%Y-%m-%d %T '

# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# enable vi keyboard bindings
set -o vi

# make less more friendly for non-text input files
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

git_branch() {
    git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# set prompt
PS1="\[\033[01;32m\]\u\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\[\033[01;32m\]\$(git_branch)\[\033[00m\] λ "

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        source /etc/bash_completion
    fi
fi

# add aliases
[ -f "$HOME/.bash_aliases" ] && source "$HOME/.bash_aliases"

# add fzf support
eval "$(fzf --bash)"

# add vterm support
if [ "$INSIDE_EMACS" = vterm ] &&
    [ -n "$EMACS_VTERM_PATH" ] &&
    [ -f "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh" ]; then
    source "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh"
fi

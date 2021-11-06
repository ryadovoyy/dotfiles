# if not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# set history length
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS
shopt -s checkwinsize

# make less more friendly for non-text input files
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

git_branch () {
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

if [ "$color_prompt" = yes ]; then
    # PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u\[\033[00m\] \[\033[01;34m\]\w\[\033[00m\]\[\033[01;32m\]\$(git_branch)\[\033[00m\] \$ "
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# if this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and add aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# add more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# add an "alert" alias for long running commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# import colorscheme from wal 
(cat ~/.cache/wal/sequences &)

# set custom environment variables
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS"
    --color=fg:7,hl:11
    --color=fg+:7,bg+:0,hl+:11
    --color=info:8,prompt:11,pointer:11
    --color=marker:8,spinner:8,header:8
"
export PF_INFO="ascii title os kernel uptime pkgs shell wm editor memory"
export EDITOR="nvim"

# add fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

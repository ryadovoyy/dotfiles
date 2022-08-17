#!/bin/sh

# improve verbosity
alias \
    cp="cp -iv" \
    mv="mv -iv" \
    rm="rm -Iv" \
    mkdir="mkdir -v" \
    bc="bc -q"

# color output
alias \
    ls="ls -h --group-directories-first --color=auto" \
    grep="grep --color=auto" \
    diff="diff --color=auto" \
    ip="ip -color=auto"

# send a notification after a long running command execution
# example: [command]; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history | tail -n1 | sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# copy an entry's password to the clipboard
# format: kp [entry] [timeout in seconds]
alias kp="keepassxc-cli clip -k ~/Documents/database.key ~/Documents/database.kdbx"

# transmission
alias tmr="transmission-remote"

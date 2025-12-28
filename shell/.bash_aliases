#!/bin/sh

# improve verbosity
alias \
    cp='cp -v' \
    mv='mv -v' \
    rm='rm -Iv' \
    mkdir='mkdir -v' \
    bc='bc -q'

# color output
alias \
    ls='ls -lAh --group-directories-first --color=auto' \
    grep='grep --color=auto' \
    diff='diff --color=auto' \
    ip='ip -color=auto'

# send a notification after a long running command execution
# example: [command]; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history | tail -n1 | sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# copy an entry's password to the clipboard
# format: kp [entry] [timeout in seconds]
alias kp="keepassxc-cli clip -k $HOME/Documents/database.key $HOME/Documents/database.kdbx"

# transmission
alias trem='transmission-remote'

# zellij
alias \
    zj='zellij' \
    zjn='zellij options --default-layout compact --theme ansi' \
    zja='zjn --attach-to-session true --session-name'

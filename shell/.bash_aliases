#!/bin/sh

# improve verbosity
alias \
    cp='cp -v' \
    mv='mv -v' \
    rm='rm -Iv' \
    mkdir='mkdir -v' \
    bc='bc -q'

# improve output
alias \
    ls='ls -lAh --group-directories-first --color=auto' \
    grep='grep --color=auto' \
    diff='diff --color=auto' \
    ip='ip -color=auto'

# copy an entry's password to the clipboard
# format: kp [entry] [timeout in seconds]
alias kp="keepassxc-cli clip -k $HOME/Documents/database.key $HOME/Documents/database.kdbx"

# transmission
alias trem='transmission-remote'

# emacs
alias emacs='emacs -nw'

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

[ -d "$HOME/.local/bin" ] && PATH="$PATH:$HOME/.local/bin"
[ -d "$HOME/go" ] && PATH="$PATH:$HOME/go/bin:/usr/local/go/bin"

export PATH="$PATH:$HOME/scripts"
export PF_INFO="ascii title os kernel uptime pkgs shell wm editor memory"
export EDITOR=nvim
export FZF_DEFAULT_OPTS=$FZF_DEFAULT_OPTS"
    --color=fg:7,hl:11
    --color=fg+:7,bg+:0,hl+:11
    --color=info:8,prompt:11,pointer:11
    --color=marker:8,spinner:8,header:8
"

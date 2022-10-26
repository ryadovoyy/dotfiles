[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

[ -d "$HOME/.local/bin" ] && PATH="$PATH:$HOME/.local/bin"
[ -d "$HOME/go" ] && PATH="$PATH:$HOME/go/bin:/usr/local/go/bin"

export PATH="$PATH:$HOME/scripts"
export PF_INFO='ascii title os kernel uptime pkgs shell wm editor memory'
export EDITOR=nvim
export FZF_ALT_C_COMMAND="fd --type d --hidden --exclude .git . $HOME"
export FZF_CTRL_T_COMMAND="fd --hidden --exclude .git . $HOME"
export FZF_DEFAULT_OPTS='
    --color=fg:7,hl:2
    --color=fg+:7,bg+:8,hl+:2
    --color=info:8,prompt:5,pointer:8
    --color=marker:5,spinner:8,header:8
'

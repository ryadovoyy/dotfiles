[ -d "$HOME/.local/bin" ] && PATH="$PATH:$HOME/.local/bin"
[ -d "$HOME/.cargo/bin" ] && PATH="$PATH:$HOME/.cargo/bin"
[ -d "$HOME/go/bin" ] && PATH="$PATH:$HOME/go/bin"
[ -d '/usr/local/go/bin' ] && PATH="$PATH:/usr/local/go/bin"

export PATH="$PATH:$HOME/scripts"
export EDITOR=hx
export FZF_ALT_C_COMMAND='fd --type d --hidden --exclude .git .'
export FZF_CTRL_T_COMMAND='fd --hidden --exclude .git .'
export FZF_DEFAULT_OPTS='
    --color=fg:7,fg+:7,hl:2,hl+:2,prompt:5,marker:5,query:15
    --color=bg+:8,pointer:8,spinner:8,info:8,header:8,border:8
'

[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"

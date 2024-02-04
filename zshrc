
export EXA_COLORS="di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32"

alias sl="exa"
alias ls="exa"
alias l="exa -l"
alias la="exa -la"
alias c="clear"
alias .="cd .."
alias vim="nvim"
alias gcmsg="git commit -m"
alias gst="git status"

PROMPT="%F{blue}%2~%f"$'\n'"%F{green}❯%f "

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^[[A' history-beginning-search-backward-end
bindkey '^[[B' history-beginning-search-forward-end
bindkey '^P' history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

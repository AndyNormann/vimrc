
export LS_COLORS="di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32"
export EDITOR=nvim

alias sl="lsd"
alias ls="lsd"
alias l="lsd -l"
alias la="lsd -la"
alias c="clear"
alias .="cd .."
alias cd="z"
alias vim="nvim"
alias vi="nvim"
alias v="nvim"
alias gd="git difftool --no-symlinks --dir-diff"
alias gco="git checkout"
alias gcm="git checkout main"
alias gcd="git checkout develop"
alias gp="git pull"
alias gcmsg="git commit -m"
alias gst="git status"
alias jai="~/.scripts/jai/bin/jai-macos"

function parse_git_branch() {
    git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
}

autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

bindkey '^[[A' history-beginning-search-backward-end
bindkey '^[[B' history-beginning-search-forward-end
bindkey '^P' history-beginning-search-backward-end
bindkey '^N' history-beginning-search-forward-end

eval "$(zoxide init zsh)"
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix'
export FZF_DEFAULT_OPTS="--history=$HOME/.fzf_history"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"

# bun completions
[ -s "/Users/andreasfladstad/.bun/_bun" ] && source "/Users/andreasfladstad/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"

setopt PROMPT_SUBST

export PROMPT="%F{blue}%2~%f "$'%F{green}$(parse_git_branch)%f'$'\n'"%F{green}❯%f "

precmd () {print -Pn "\e]0;%~\a"}


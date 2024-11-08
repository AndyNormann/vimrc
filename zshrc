# export LS_COLORS="di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32"
# export EXA_COLORS=di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32
# export LS_COLORS=di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32
# export EZA_COLORS="di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32"
export EZA_COLORS="reset:*.json=33:di=34:bd=33:cd=33:so=31:ex=32:ur=33:uw=31:ux=32:ue=32:uu=33:gu=33:lc=31:df=32:sn=32:nb=32:nk=32:nm=32:ng=32:nt=32"

export EDITOR=nvim

export HISTFILESIZE=1000000000
export HISTSIZE=1000000000
export HISTFILE=~/.zsh_history
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt SHARE_HISTORY

alias sl="eza"
alias ls="eza"
alias l="eza -l"
alias la="eza -la"
alias ll="eza"
alias lls="eza"
alias tree="eza --tree"
alias c="clear"
alias .="cd .."
alias vim="nvim"
alias vi="nvim"
alias v="nvim"
alias gd="git difftool --no-symlinks --dir-diff"
alias gco="git checkout"
alias gcm="git checkout main"
alias gcd="git checkout develop"
alias gp="git pull --rebase"
alias gcmsg="git commit -m"
alias gst="git status"
alias jai="~/.scripts/jai/bin/jai-macos"

function cd() { builtin cd $1 && zoxide add . }

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
source <(fzf --zsh)

setopt PROMPT_SUBST

export PROMPT="%F{blue}%2~%f "$'%F{green}$(parse_git_branch)%f'$'\n'"%F{green}❯%f "

precmd () {print -Pn "\e]0;%~\a"}


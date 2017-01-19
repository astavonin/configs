source ~/.zsh_local.sh

export PATH=/opt/local/bin:/opt/local/sbin:$PATH

alias ll='ls -l -G'
alias cd..="cd .."
alias l="ls -al"
alias lp="ls -p"
alias h=history
alias cp='nocorrect cp -v'
alias grep="grep -i --color=always"

autoload -U compinit
compinit

bindkey '^?' backward-delete-char

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
zstyle ':completion:*' menu select

setopt correctall
setopt cdablevars

HISTSIZE=500
SAVEHIST=500
HISTFILE=~/.zsh_history
setopt append_history
setopt inc_append_history
setopt extended_history
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_reduce_blanks
setopt hist_ignore_space
setopt hist_no_store
setopt hist_no_functions
setopt no_hist_beep
setopt hist_save_no_dups

export PS1="%n:%-1d/../%1d: "

alias pg='ps aux | grep '
alias vim='nocorrect vim'
alias e='emacs'
g()
{
	mdfind -onlyin . $1 -0 | xargs -0 grep -n --color=always $1
}
f(){find . -name '*'$1'*' -print}


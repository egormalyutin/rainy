# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias l='ls -lah'
alias md='mkdir -p'
PS1="\n\[\e[34m\]\$(git rev-parse --is-inside-work-tree 2>/dev/null >/dev/null || dirs +0)\[\e[1m\]\$(git rev-parse --show-toplevel 2>/dev/null | xargs -r basename)\[\e[0m\e[34m\]\$(git rev-parse --show-prefix 2>/dev/null | sed -r '/^\s*$/d' | sed 's/^/\//' | sed 's/\/$//')\[\e[0m\e[90m\]\$(git rev-parse --abbrev-ref HEAD 2>/dev/null | sed 's/^/ \(/' | sed 's/$/)/')\n\[\e[35m\]⬢\[\e[0m\] "

shopt -s histverify

bind '"\t":menu-complete'
bind "set show-all-if-ambiguous on"
bind "set completion-ignore-case on"
bind "set menu-complete-display-prefix on"

bind '"\e[A": history-search-backward'
bind '"\eOA": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\eOB": history-search-forward'

shopt -s histappend
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

HISTSIZE=-1
HISTFILESIZE=-1

source ~/bashrc-2

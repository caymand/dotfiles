#Alias
alias ec='emacsclient -nw'
alias arun=acme_run
alias vi=nvim
alias python3=python3.10
alias maple=/Library/Frameworks/Maple.framework/Versions/2022/bin/maple
alias ..="cd .."
alias ....=".. && .."
alias mtm="mtm -c b"
alias reboot="sudo reboot"

PS1='$(exit_code=$?; [[ $exit_code -eq 0 ]] || printf $exit_code)'\
'[\[\033[00;31m\]\h'\
'\[\033[00m\]]'\
'\[\033[00;32m\]\w\[\033[00m\]>\[\033[01;33m\]\$ \[\033[00m\]'


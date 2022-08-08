# Path
ATH=$PATH:$HOME/.emacs.d/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:/usr/local/plan9/bin
PATH=$PATH:$HOME/.ghcup/bin

#Sources
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zsh-z/zsh-z.plugin.zsh

#Env vars
export EDITOR='emacs'
export CLICOLOR=1

#Prompt look
PROMPT='%F{green}%m%f :: %F{blue}%2~%f %# '

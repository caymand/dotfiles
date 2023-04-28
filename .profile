. "$HOME/.cargo/env"

PLAN9=/usr/local/plan9

# Path
PATH=$PATH:$PLAN9/bin
PATH=$PATH:$HOME/.emacs.d/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.ghcup/bin
PATH=$PATH:$HOME/.ghcup/bin
PATH=$PATH:/usr/local/texlive/2022basic/bin/universal-darwin
PATH="$PATH:/usr/local/bin"

# Exports
export HISTSIZE=1000000
export HISTFILESIZE=10000000
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export PATH
export PLAN9
export CLICOLOR=1
export EDITOR='mg'
export CLICOLOR=1
export PLAN9=/usr/local/plan9
export ZONE="europe-west1-b"
export INSTANCE_NAME="basic-ml"
export NAMESPACE="$HOME/p9srv"
if [ $(uname) = Darwin ]; then
   export TOOLCHAINS=$(plutil -extract CFBundleIdentifier raw /Library/Developer/Toolchains/swift-5.7.3-RELEASE.xctoolchain/Info.plist)
fi
export SHELL=/usr/local/bin/bash

eval $(thefuck --alias)

# configure links
ln -nsf ~/.zshrc.d/pure/pure.zsh /usr/local/share/zsh/site-functions/prompt_pure_setup >/dev/null
ln -nsf ~/.zshrc.d/pure/async.zsh /usr/local/share/zsh/site-functions/async >/dev/null

# initialize the prompt system (if not so already) and choose pure
autoload -U promptinit; promptinit
prompt pure

# extended tab completion
autoload -U compinit
compinit

# case insensitve tab completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# extra tab completion
fpath=(~/.zshrc.d/completions/src $fpath)

# better history searching with arrow keys
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# save command distory
HISTFILE=~/.zsh_history
HISTSIZE=SAVEHIST=10000
setopt sharehistory
setopt extendedhistory

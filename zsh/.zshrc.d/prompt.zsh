# Initialise zplug: https://github.com/zplug/zplug
export ZPLUG_HOME=/usr/local/opt/zplug
source $ZPLUG_HOME/init.zsh

# Source asdf version manager and add completions
zplug "plugins/asdf",   from:oh-my-zsh

# Add completions for the gem command
zplug "plugins/gem",    from:oh-my-zsh

# Add wd: https://github.com/mfaerevaag/wd
zplug "plugins/wd",     from:oh-my-zsh

# Add zsh autosuggestions
zplug "zsh-users/zsh-autosuggestions"

# Add zsh completions
zplug "zsh-users/zsh-completions"

# Add up, down arrow history search
zplug "zsh-users/zsh-history-substring-search"

# Set theme: https://github.com/denysdovhan/spaceship-prompt
# zplug denysdovhan/spaceship-prompt, use:spaceship.zsh, from:github, as:theme
# SPACESHIP_DIR_TRUNC=0
# SPACESHIP_PACKAGE_SHOW=false
# SPACESHIP_NODE_SHOW=false
# SPACESHIP_RUBY_SHOW=false

zplug mafredri/zsh-async, from:github
zplug sindresorhus/pure, use:pure.zsh, from:github, as:theme

# [MUST BE LOADED LAST] Add zsh syntax highlighting
zplug "zsh-users/zsh-syntax-highlighting"

# Source zplug plugins and add commands to $PATH
zplug load

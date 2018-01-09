#!/bin/sh

# clean up any initial configuration
rm -f ~/.zshrc
rm -f ~/.bashrc
rm -f ~/.bash_profile
rm -f ~/.bash_history

# -R = restow (delete and then stow)
# -v = be verbose
stow -vR zsh
stow -vR git
stow -vR ruby
stow -vR emacs

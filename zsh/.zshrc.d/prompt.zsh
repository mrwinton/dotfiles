# configure links
ln -nsf ~/.zshrc.d/pure/pure.zsh /usr/local/share/zsh/site-functions/prompt_pure_setup >/dev/null
ln -nsf ~/.zshrc.d/pure/async.zsh /usr/local/share/zsh/site-functions/async >/dev/null

# initialize the prompt system (if not so already) and choose pure
autoload -U promptinit; promptinit
prompt pure

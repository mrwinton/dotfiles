# -*- mode: sh -*-

alias ag="ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'"
alias gco="git checkout"
alias gm="git merge"
alias gc="git commit -v"
alias gst="git status"
alias gdc="git diff --cached"
alias glo="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias la="ls -la"
alias ll="ls -l"
alias ln="ln -v"
alias ls="ls -h"
alias mkdir="mkdir -p"

usage() {
  du -sch "$@" | sort -h
}

### Ruby/Rails-specific
alias be="bundle exec"
alias migrate="be rake db:migrate db:test:prepare"

### Networking
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias serve="python -m SimpleHTTPServer"

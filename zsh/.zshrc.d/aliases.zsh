# -*- mode: sh -*-

alias ag="ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw"
alias gco="git checkout"
alias gm="git merge"
alias gc="git commit -v"
alias gst="git status"
alias gdc="git diff --cached"
alias glo="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias la="ls -la"
alias ll="ls -l"
alias ln="ln -v"
alias ls="ls -Gh"
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

### Flux
# alias sourcetree='open -a SourceTree .'
# alias be='bundle exec'

# alias nz='PS_MARKET=nz bundle exec'
# alias nzs='PS_MARKET=nz bundle exec spring'

# alias au='PS_MARKET=au bundle exec'
# alias aus='PS_MARKET=au bundle exec spring'

# alias uk='PS_MARKET=uk bundle exec'
# alias uks='PS_MARKET=uk bundle exec spring'

# alias compress_nz='nz rake db:compress\[9999\]'
# alias compress_au='au rake db:compress\[9999\]'
# alias compress_uk='uk rake db:compress\[9999\]'

# alias ps_prod='git fetch && git describe --abbrev=0 origin/current-nz-prod-app --tags'
# alias spring_stahp='nz spring stop && au spring stop && uk spring stop'

# alias cdps='cd $HOME/Development/powershop/'
# alias cdpsbau='cd $HOME/Development/powershop-banking-au/'
# alias cdpss='cd $HOME/Development/powershop-scripts/'

# alias aussie='au rails s -b 127.0.0.1'
# alias kiwi='nz rails s -b 127.0.0.1'
# alias england='uk rails s -b 127.0.0.1'
# alias ausetup='bundle; spring_stahp; au rake db:migrate; au rake db:test:nuke'
# alias nzsetup='bundle; spring_stahp; nz rake db:migrate; nz rake db:test:nuke'
# alias uksetup='bundle; spring_stahp; uk rake db:migrate; uk rake db:test:nuke'

# alias ret='RAILS_ENV=test'

# eval "$(rbenv init -)"

# alias reset_banking='bundle; be rails db:drop; be rails db:create; be rails db:migrate; be rails db:test:prepare'

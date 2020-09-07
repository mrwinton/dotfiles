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
alias glr="git branch --sort='-committerdate' --format='%(color:green)%(committerdate:relative)%(color:reset) %(refname:short)'"
alias la="ls -la"
alias ll="ls -l"
alias ln="ln -v"
alias ls="ls -Gh"
alias mkdir="mkdir -p"

### Ruby/Rails-specific
alias be="bundle exec"
alias migrate="be rake db:migrate db:test:prepare"
alias reset="be rake db:drop db:create db:migrate"
alias packages="bundle install; yarn install"
alias seed="be rake db:seed"
alias sync="bundle exec rake i18nlite:sync"
alias ret="RAILS_ENV=test"
alias red="RAILS_ENV=development"
alias tint="SHOW_BROWSER=1 DEBUG_BROWSER=1"

### Networking
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias serve="python -m SimpleHTTPServer"

### Promote
alias vup="vagrant up gud.vagrant; vagrant ssh gud.vagrant -c 'sudo service gud restart; (cd /vagrant/repos/promote-dispatcher; bundle exec ruby bin/dispatcher.rb &); (cd /vagrant/repos/promote-gud/; ./bin/reaperctl start &); cat'"
alias clean="packages && reset && seed && sync; ret reset;"
alias dcdown="docker images | awk '{print $3}' | xargs docker image rm; docker-compose down --remove-orphans"
alias dcpup="ansible-playbook setup_local_dev_env.yaml --tag promote -e 'autorm=no'"
alias dcgup="ansible-playbook setup_local_dev_env.yaml --tag gud -e 'autorm=no'"

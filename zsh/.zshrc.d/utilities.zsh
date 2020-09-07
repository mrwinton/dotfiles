# -*- mode: sh -*-

function countpage() {
    pdf2dsc "$1" /dev/stdout | grep "Pages" | sed s/[^0-9]//g
}

function path() {
    echo $PATH | tr ':' '\n'
}

function usage() {
    du -sch "$@" | sort -h
}

function active_branch {
  echo $(git_current_branch | tr -d "[[:space:]]")
}

function active_branch_cleaned {
  echo $(git_current_branch | tr "[:upper:]" "[:lower:]" | sed "s/[^0-9a-z_-]//g")
}

function graft_branch {
  local branch=${1:-$(active_branch_cleaned)}
  curl --fail -H "Api-Token: $(pass grafter_token)" "http://grafter.dev.promoteapp.net:1414/${branch}"
}

function graft_tail {
  local branch=${1:-$(active_branch_cleaned)}
  local grafter="/home/promote/apps/grafter/release_branch.log"
  local branch_log="/home/promote/apps/promote-release/tmp/branch-${branch}.log /opt/promote/${branch}/shared/log/\*.log"
  ssh grafter "tail -n 100 -F $grafter $branch_log"
}

function graft_flag {
  local branch=${1:-$(active_branch_cleaned)}
  vim scp://grafter//opt/promote/${branch}/shared/config/features_${branch}.yaml

  graft_restart $branch
}

function graft_conf {
  local branch=${1:-$(active_branch_cleaned)}
  vim scp://grafter//opt/promote/${branch}/shared/config/production_${branch}.yaml

  graft_restart $branch
}

function graft_restart {
  local branch=${1:-$(active_branch_cleaned)}
  ssh grafter -t "sudo -u root bash -ilc '/etc/init.d/unicorn_init_${branch} stop; sleep 5; /etc/init.d/unicorn_init_${branch} start; /etc/init.d/delayed_job_${branch} restart; /etc/init.d/sidekiq-${branch} stop; sleep 2; /etc/init.d/sidekiq-${branch} start;'"
}

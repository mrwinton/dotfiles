{ config, pkgs, ... }:

with pkgs;

{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;

    initExtra = ''
    source $HOME/.nix-profile/asdf/asdf.sh
    PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"
    export LANG="en_GB"
    export LC_ALL="en_GB.UTF-8"
    export LC_CTYPE="en_GB.UTF-8"

    eval "$(starship init zsh)"

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
    '';

    history = {
      path = "$HOME/.zsh_history";
      size = 50000;
      save = 50000;
    };

    shellAliases = {
      ag = "ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'";

      be = "bundle exec";
      bi = "bundle install";
      bo = "bundle open";
      bu = "bundle update";

      cat = "bat";

      gco = "git checkout";
      gm = "git merge";
      gc = "git commit -v";
      gst = "git status";
      gdc = "git diff --cached";
      glo = "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      glr = "git branch --sort='-committerdate' --format='%(color:green)%(committerdate:relative)%(color:reset) %(refname:short)'";

      la = "ls -la";
      ll = "ls -l";
      ln = "ln -v";
      
      mkdir = "mkdir -p";

      vim = "emacsclient -nw";
      e   = "emacsclient -nw";
    };

    plugins = [
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.5.0";
          sha256 = "19qkg4b2flvnp2l0cbkl4qbrnl8d3lym2mmh1mx9nmjd7b81r3pf";
        };
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-syntax-highlighting";
          rev = "v0.6.0";
          sha256 = "0zmq66dzasmr5pwribyh4kbkk23jxbpdw4rjxx0i7dx8jjp2lzl4";
        };
      }
    ];
  };
}

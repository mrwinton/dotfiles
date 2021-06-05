{ config, pkgs, ... }:

with pkgs;

{
  home.file = {
    p10k = {
      source = ./p10k.zsh;
      target = ".p10k.zsh";
    };
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;

    initExtra = ''
      [[ ! -f ~/.zshrc.local ]] || source ~/.zshrc.local

      # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

      function path() {
        echo $PATH | tr ':' '\n'
      }

      function em {
        emacs $argv >/tmp/emacs.log 2>&1 &
      }

      function ports {
        lsof -i 4 -P -n | grep -i 'listen'
      }

      function rebuild {
        darwin-rebuild switch -I darwin-config=$HOME/src/dotfiles/darwin.nix
      }

      function update {
        nix-channel --update; rebuild;
      }

      function clean {
        nix-collect-garbage -d
      }
    '';

    envExtra = ''
      typeset -U PATH
      PATH="$PATH:$HOME/.bin"
      PATH="$PATH:/run/current-system/sw/bin:$PATH"
      PATH="$PATH:/usr/sbin"
      PATH="/Applications/Postgres.app/Contents/Versions/latest/bin:$PATH"
      PATH=".git/safe/../../bin:$PATH"
      export PATH

      export LANG="en_GB"
      export LC_ALL="en_GB.UTF-8"
      export LC_CTYPE="en_GB.UTF-8"

      source $HOME/.nix-profile/asdf/asdf.sh
    '';

    history = {
      path = "$HOME/.zsh_history";
      size = 50000;
      save = 50000;
    };

    shellAliases = {
      ag =
        "ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'";
      be = "bundle exec";
      gst = "git status";
      glo =
        "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      glr =
        "git branch --sort='-committerdate' --format='%(color:green)%(committerdate:relative)%(color:reset) %(refname:short)'";
      la = "ls -la";
      mkdir = "mkdir -p";
      vim = "emacsclient -nw";
      e = "emacsclient -nw";
    };

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
    };

    plugins = [
      {
        name = "powerlevel10k";
        src = pkgs.zsh-powerlevel10k;
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
      }
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

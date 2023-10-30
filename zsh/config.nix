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
      PATH=".git/safe/../../bin:$PATH"
      [[ ! -f ~/.zshrc.local ]] || source ~/.zshrc.local

      # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

      if command -v direnv >/dev/null; then
        eval "$(direnv hook zsh)"
      fi

      source "$(fzf-share)/key-bindings.zsh"
      source "$(fzf-share)/completion.zsh"

      function path() {
        echo $PATH | tr ':' '\n'
      }

      function em {
        emacs $argv >/tmp/emacs.log 2>&1 &
      }

      function ports {
        lsof -i 4 -P -n | grep -i 'listen'
      }

      function flake-build {
        cd ~/src/github.com/mrwinton/dotfiles; nix build .#darwinConfigurations.m-one.system
      }

      function flake-switch {
        cd ~/src/github.com/mrwinton/dotfiles; ./result/sw/bin/darwin-rebuild switch --flake ./\#m-one
      }

      function flake-clean {
        nix-collect-garbage -d
      }

      function doom-install {
        cd ~/.config/emacs/doom.emacs.d; bin/doom install
      }

      function doom-sync {
        cd ~/.config/emacs/doom.emacs.d; bin/doom sync
      }

      function doom-upgrade {
        cd ~/.config/emacs/doom.emacs.d; bin/doom upgrade
      }
    '';

    envExtra = ''
      typeset -U PATH
      PATH="$PATH:$HOME/.bin"
      PATH="$PATH:/run/current-system/sw/bin:$PATH"
      PATH="$PATH:/usr/sbin"
      export PATH

      export LANG="en_US"
      export LC_ALL="en_US.UTF-8"
      export LC_CTYPE="en_US.UTF-8"
      export GIT_SSL_CAINFO="/etc/ssl/certs/ca-certificates.crt"
    '';

    history = {
      path = "$HOME/.zsh_history";
      size = 50000;
      save = 50000;
    };

    shellAliases = {
      ag =
        "ag --color --color-line-number '0;35' --color-match '46;30' --color-path '4;36'";
      bat = "bat --theme='OneHalfLight'";
      clone = "ghq get";
      e = "emacsclient -nw";
      emacs_gui = "/Applications/Emacs.app/Contents/MacOS/Emacs";
      gst = "git status";
      glo =
        "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      glr =
        "git branch --sort='-committerdate' --format='%(color:green)%(committerdate:relative)%(color:reset) %(refname:short)'";
      j = "cd $(ghq list -p | fzf -1 -e)";
      la = "ls -la";
      mkdir = "mkdir -p";
      vim = "emacsclient -nw";
      rspec = "bundle exec rspec --color --format documentation";
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

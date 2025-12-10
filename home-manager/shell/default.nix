{ ... }:
{
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestion.enable = true;

    sessionVariables = {
      PATH = ".git/safe/../../bin:/opt/homebrew/bin:$HOME/.local/bin:$PATH";
    };

    initExtra = ''
      [[ ! -f ~/.zshrc.local ]] || source ~/.zshrc.local

      if command -v direnv >/dev/null; then
        eval "$(direnv hook zsh)"
      fi

      function path {
        echo $PATH | tr ':' '\n'
      }

      function ports {
        lsof -i 4 -P -n | grep -i 'listen'
      }

      function dotfiles-update {
        sudo darwin-rebuild switch --flake ~/src/github.com/mrwinton/dotfiles
      }
    '';

    history = {
      path = "$HOME/.zsh_history";
      size = 50000;
      save = 50000;
    };

    shellAliases = {
      clone = "ghq get";
      gst = "git status";
      glo =
        "git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      glr =
        "git branch --sort='-committerdate' --format='%(color:green)%(committerdate:relative)%(color:reset) %(refname:short)'";
      j = "cd $(ghq list -p | fzf -1 -e)";
      la = "ls -la";
      mkdir = "mkdir -p";
      vim = "emacsclient -nw";
    };
  };
  programs.starship = {
    enable = true;

    settings = {
      command_timeout = 100;
      format = "[$all](dimmed white)";

      directory.style = "italic white";

      git_branch.ignore_branches = [ "master" "main" ];

      git_status = {
        style = "bold yellow";
        format = "([$all_status$ahead_behind]($style) )";
      };

      elixir.symbol = " ";
      lua.symbol = "󰢱 ";
      nix_shell.symbol = " ";
      ruby.symbol = " ";

      character = {
        success_symbol = "[❯](dimmed green)";
        error_symbol = "[❯](dimmed red)";
      };

      jobs.disabled = true;
    };
  };
}

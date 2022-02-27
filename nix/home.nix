{ config, lib, pkgs, ... }:
{
  programs.home-manager.enable = true;

  imports = [
    ../emacs/config.nix
    ../git/config.nix
    ../hammerspoon/config.nix
    ../ruby/config.nix
    ../zsh/config.nix
  ];

  # https://search.nixos.org/packages?channel=unstable
  home.packages = with pkgs; [
    bat
    coreutils-full
    emacsGcc
    fd
    fzf
    ghq
    git
    heroku
    nixpkgs-fmt
    openssl.dev
    proselint
    ripgrep
    youtube-dl
    universal-ctags
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.username = "michaelwinton";
  home.stateVersion = "22.05";
}

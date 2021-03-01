{ config, pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  asdf = callPackage ~/src/dotfiles/asdf/package.nix { };
  comma = callPackage ~/src/dotfiles/comma/package.nix { };
in {
  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;

  imports = [
    ./asdf/config.nix
    ./emacs/config.nix
    ./git/config.nix
    ./kitty/config.nix
    ./ruby/config.nix
    ./zsh/config.nix
  ];

  # https://search.nixos.org/packages?channel=unstable
  home.packages = with pkgs; [
    asdf
    aspell
    autoconf
    automake
    bat
    coreutils-prefixed
    comma
    git
    gitAndTools.gh
    gitAndTools.hub
    heroku
    imagemagick
    nixfmt
    openssl.dev
    ripgrep
    starship
    tmux
    youtube-dl
    universal-ctags
    zsh
  ];

  home.username = "michaelwinton";
  home.homeDirectory = "/Users/michaelwinton";
  home.stateVersion = "21.05";
}

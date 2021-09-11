{ config, lib, pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  comma = callPackage ./comma/package.nix { };
  hammerspoon = callPackage ./hammerspoon/package.nix { };

  # https://github.com/nix-community/emacs-overlay
  emacs_sha = "4947b85a89f10799ef9b8bc1ee924c47f7919363";
in {
  programs.home-manager.enable = true;

  imports = [
    ./emacs/config.nix
    ./git/config.nix
    ./hammerspoon/config.nix
    ./kitty/config.nix
    ./ruby/config.nix
    ./zsh/config.nix
  ];

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/${emacs_sha}.tar.gz";
    }))
  ];

  # https://search.nixos.org/packages?channel=unstable
  home.packages = with pkgs; [
    autoconf
    automake
    bat
    pkgs.chromedriver
    pkgs.coreutils-full
    comma
    emacsGcc
    fd
    fzf
    geckodriver
    ghq
    pkgs.git
    pkgs.gitAndTools.gh
    pkgs.gitAndTools.hub
    hammerspoon
    heroku
    imagemagick
    nixfmt
    openssl.dev
    proselint
    ripgrep
    stripe-cli
    tmux
    youtube-dl
    universal-ctags
    pkgs.zsh
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.activation.copyApplications = let
    apps = pkgs.buildEnv {
      name = "home-manager-applications";
      paths = config.home.packages;
      pathsToLink = "/Applications";
    };
  in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    baseDir="/Applications/Home Manager"
    if [ -d "$baseDir" ]; then
      $DRY_RUN_CMD sudo rm -rf "$baseDir"
    fi
    sudo mkdir -p "$baseDir"
    for appFile in ${apps}/Applications/*; do
      target="$baseDir/$(basename "$appFile")"
      sudo cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
      sudo chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
    done
  '';

  home.username = "michaelwinton";
  home.homeDirectory = "/Users/michaelwinton";
  home.stateVersion = "21.05";
}

{ config, lib, pkgs, ... }:

let
  callPackage = pkgs.callPackage;

  asdf = callPackage ~/src/dotfiles/asdf/package.nix { };
  comma = callPackage ~/src/dotfiles/comma/package.nix { };
in {
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
    proselint
    ripgrep
    tmux
    youtube-dl
    universal-ctags
    zsh
  ];

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

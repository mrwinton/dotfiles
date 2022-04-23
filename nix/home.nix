{ config, lib, pkgs, ... }:
let
  aspellEnv = pkgs.aspellWithDicts (d: [ d.en d.sv ]);
in
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
    aspellEnv
    bat
    coreutils-full
    emacsGcc
    fd
    fzf
    ghq
    git
    heroku
    imagemagick
    nixpkgs-fmt
    openssl.dev
    proselint
    ripgrep
    youtube-dl
    universal-ctags
  ];

  home.file.".aspell.conf".text = "data-dir ${aspellEnv}/lib/aspell";

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  home.activation.copyApplications =
    let
      apps = pkgs.buildEnv {
        name = "home-manager-applications";
        paths = config.home.packages;
        pathsToLink = "/Applications";
      };
    in
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
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
  home.stateVersion = "22.05";
}

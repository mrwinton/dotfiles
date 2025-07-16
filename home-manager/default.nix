{ config, lib, pkgs, ... }:
let
  aspellEnv = pkgs.aspellWithDicts (d: [ d.en d.sv ]);
in
{
  imports = [
    ./emacs
    ./shell
  ];

  xdg = {
    enable = true;
    configFile = {
      "irbrc".source = ./../.config/irbrc;
      "ghostty/config".source = ./../.config/ghostty/config;
      "git/config".source = ./../.config/git/config;
      "git/excludes".source = ./../.config/git/excludes;
      "git/config-local.sample".source = ./../.config/git/config-local.sample;
    };
  };

  home = {
    stateVersion = "22.05";

    packages = with pkgs; [
      aspellEnv
      bat
      clang
      claude-code
      comma
      coreutils-full
      editorconfig-core-c
      enchant
      fd
      fontconfig
      fzf
      ghq
      git
      git-open
      git-recent
      git-trim
      gnugrep
      helix
      imagemagick
      jsbeautifier
      nixfmt-rfc-style
      nixpkgs-fmt
      openssl.dev
      pandoc
      pgformatter
      proselint
      ripgrep
      shellcheck
      shfmt
      stylelint
      universal-ctags
    ];

    file = {
      ".aspell.conf".text = "data-dir ${aspellEnv}/lib/aspell";
      ".hammerspoon".source = ./../.config/hammerspoon;
    };
  };

  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    git = {
      enable = true;
      lfs.enable = true;
    };
  };
}
